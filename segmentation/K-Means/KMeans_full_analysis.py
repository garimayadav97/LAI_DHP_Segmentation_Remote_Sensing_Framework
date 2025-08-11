import os
import cv2
import numpy as np
import pandas as pd
from sklearn.cluster import KMeans
from PIL import Image, ExifTags
import mahotas


base_folder = r"./research/dhp_analysis/data/knn-left"
excel_path = os.path.join(r'./research/dhp_analysis', r'pixel_data_layers.xlsx')
result_summary = []


output_base_dir = os.path.join(base_folder, 'segmentation_outputs')

os.makedirs(output_base_dir, exist_ok=True)


def adjust_gamma(image, gamma=1.0):
    invGamma = 1.0 / gamma
    table = np.array([((i / 255.0) ** invGamma) * 255 for i in np.arange(256)]).astype("uint8")
    return cv2.LUT(image, table)

def high_pass_filter(image, kernel_size=(11, 11), alpha=1.5, beta=-0.6):
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    low_pass = cv2.GaussianBlur(gray, kernel_size, 0)
    high_pass = cv2.addWeighted(gray, alpha, low_pass, beta, 0)
    return high_pass

def get_pixel_accuracy(excel_path, segmented_image, image_name):
    df = pd.read_excel(excel_path, sheet_name='Sheet1')
    if image_name not in df['Image'].values:
        return np.nan
    match_count, total = 0, 0
    for idx, row in df[df['Image'] == image_name].iterrows():
        x, y = int(row['X']), int(row['Y'])
        print(x,y)
        if 0 <= x < segmented_image.shape[1] and 0 <= y < segmented_image.shape[0]:
            print(row['PixelValue'],segmented_image[y, x])
            if row['PixelValue'] == segmented_image[y, x]:
                match_count += 1
            total += 1
    return (match_count / total) * 100 if total > 0 else np.nan

def segment_kmeans(image, bands='rgb'):
    h, w, _ = image.shape
    center = (w // 2, h // 2)
    radius = min(center)
    mask = np.zeros((h, w), dtype=np.uint8)
    cv2.circle(mask, center, radius, 255, -1)
    masked = cv2.bitwise_and(image, image, mask=mask)

    R, G, B = masked[:, :, 0].astype(float), masked[:, :, 1].astype(float), masked[:, :, 2].astype(float)

    if bands == 'rgb':
        features = [R, G, B]
    elif bands == 'norm':
        layer1 = (R - G) / (R + G + 1e-6)
        layer2 = (G - B) / (G + B + 1e-6)
        layer3 = (R - B) / (R + B + 1e-6)
        features = [layer1, layer2, layer3]
    elif bands == 'highpass':
        hp = high_pass_filter(image)
        features = [R, G, B, hp.astype(float)]
    elif bands == 'rgb_exg_exb':
        exg = 2 * G - R - B
        exb = B - (R + G) / 2
        features = [R, G, B, exg, exb]
    else:
        raise ValueError(f"Unknown band option: {bands}")

    stacked = np.dstack(features).reshape(-1, len(features))
    kmeans = KMeans(n_clusters=2, random_state=42, n_init=10).fit(stacked)
    clustered = kmeans.labels_.reshape(h, w)

    canopy_cluster = np.argmin(kmeans.cluster_centers_[:, 0])
    binary_mask = (clustered == canopy_cluster).astype(np.uint8) * 255
    inverted_mask = cv2.bitwise_not(binary_mask)

    return inverted_mask

def segment_riddler(image):
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    T_rc = mahotas.rc(gray)
    binary = (gray > T_rc).astype(np.uint8) * 255
    inverted_binary = cv2.bitwise_not(binary)
    return inverted_binary

# -------------------------------
# 📊 Methods to Run
# -------------------------------
methods = [
    #('K-Means', 'rgb', 'R, G, B'),
    #('K-Means', 'norm', 'Normalized Bands, R, G, B'),
    #('K-Means', 'highpass', 'High Pass filter, R, G, B'),
    ('K-Means', 'rgb_exg_exb', 'Excess Blue, Excess Green, R, G, B'),
    #('Riddler', 'rgb', 'R, G, B')
]


# Process each image
for root, dirs, files in os.walk(base_folder):
    for file in files:
        if file.endswith(".JPG"):
            image_path = os.path.join(root, file)
            image_name = file
            original = cv2.imread(image_path)
            if original is None:
                continue
            gamma_corrected = adjust_gamma(original.copy(), gamma=0.35)

            for method, band_key, description in methods:
                print('method:',method,"\tband key:",band_key,"\timage",image_name)
                # Define method-specific directory
                method_dir = os.path.join(output_base_dir, f"{method}_{band_key}")
                os.makedirs(method_dir, exist_ok=True)

                # # Process and save Before Gamma Correction
                # before_dir = os.path.join(method_dir, 'Before_Gamma')
                # os.makedirs(before_dir, exist_ok=True)
                # if method == 'K-Means':
                #     mask_before = segment_kmeans(original.copy(), bands=band_key)
                # else:
                #     mask_before = segment_riddler(original.copy())
                # before_path = os.path.join(before_dir, image_name)
                # cv2.imwrite(before_path, mask_before)
                # acc_before = get_pixel_accuracy(excel_path, mask_before, image_name)


                # Process and save After Gamma Correction
                after_dir = os.path.join(method_dir, 'After_Gamma')
                os.makedirs(after_dir, exist_ok=True)
                if method == 'K-Means':
                    mask_after = segment_kmeans(gamma_corrected.copy(), bands=band_key)
                else:
                    mask_after = segment_riddler(gamma_corrected.copy())
                after_path = os.path.join(after_dir, image_name)
                cv2.imwrite(after_path, mask_after)

                # acc_after = get_pixel_accuracy(excel_path, mask_after, image_name)

                # result_summary.append({
                #     'Image': image_name,
                #     'Segmentation Technique': method,
                #     'Bands Used': description,
                #     'Before Gamma Correction': round(acc_before, 3) if acc_before is not None else np.nan,
                #     'After Gamma Correction': round(acc_after, 3) if acc_after is not None else np.nan
                # })

                #print(result_summary)



# -------------------------------
# 💾 Save Final Results
# -------------------------------
# result_df = pd.DataFrame(result_summary)
# summary_path = os.path.join(base_folder, 'segmentation_accuracy_comparison.xlsx')
# result_df.to_excel(summary_path, index=False)