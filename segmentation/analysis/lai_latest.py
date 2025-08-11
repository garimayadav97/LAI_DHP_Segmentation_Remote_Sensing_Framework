import cv2
import numpy as np
import math
import os
import matplotlib.pyplot as plt
import pandas as pd
from datetime import datetime
from matplotlib.patches import Circle, Wedge

def calculate_lai_fixed_rings(image_path, clumping_index=0.8, ring_indices=[1, 2, 3], show_debug=False, apply_azimuth_mask=False, azimuth_range=(315, 45)):
    print(f"Processing {image_path}")
    image = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)
    if image is None:
        print(f"⚠️ Image not found or unreadable: {image_path}")
        return None, []

    _, binary = cv2.threshold(image, 127, 255, cv2.THRESH_BINARY)
    height, width = binary.shape
    cx, cy = width / 2, height / 2
    radius = min(cx, cy)

    y_idx, x_idx = np.indices(binary.shape)
    dx, dy = x_idx - cx, y_idx - cy
    distance = np.sqrt(dx**2 + dy**2)
    azimuth = np.degrees(np.arctan2(dy, dx)) % 360
    zenith_angle_rad = (distance / radius) * (np.pi / 2)
    zenith_angle_deg = np.degrees(zenith_angle_rad)

    inside_circle = distance <= radius
    if apply_azimuth_mask:
        az_start, az_end = azimuth_range
        azimuth_mask = (azimuth >= az_start) | (azimuth <= az_end) if az_start > az_end else (azimuth >= az_start) & (azimuth <= az_end)
        full_mask = inside_circle & azimuth_mask
    else:
        full_mask = inside_circle

    bin_edges = [0, 7, 23, 38, 53, 68]  # fixed bins matching LI-COR rings
    zenith_bins = list(zip(bin_edges[:-1], bin_edges[1:]))
    gap_fractions = []
    lai_sum = 0.0

    for i in ring_indices:
        lower, upper = zenith_bins[i]
        bin_mask = (zenith_angle_deg >= lower) & (zenith_angle_deg < upper) & full_mask
        total_pixels = np.count_nonzero(bin_mask)
        sky_pixels = np.count_nonzero((binary == 255) & bin_mask)
        gap_fraction = sky_pixels / total_pixels if total_pixels > 0 else 0
        gap_fractions.append(gap_fraction)

        if not (0 < gap_fraction < 1):
            continue

        theta_mid = math.radians((lower + upper) / 2)
        weight = math.cos(math.radians(lower)) - math.cos(math.radians(upper))
        K = -math.log(gap_fraction) * math.cos(theta_mid)
        lai_sum += K * weight

    lai_effective = 2 * lai_sum
    lai_true = lai_effective / clumping_index if clumping_index > 0 else lai_effective
    print(f"✅ LAI (rings {ring_indices}): {lai_true:.3f}")

    return lai_true, gap_fractions

def process_lai_excel_fixed_rings(excel_path, image_column, image_folder, clumping_index=0.8):
    df = pd.read_excel(excel_path)

    ring_definitions = {
        #3: [1, 2, 3],       # Rings 2–4 (23–53°)
        #4: [1, 2, 3, 4],    # Rings 2–5 (23–68°)
        5: [0, 1, 2, 3, 4]  # Rings 1–5 (7–68°)
    }

    for ring_count, indices in ring_definitions.items():
        lai_list = []
        gap_list = []

        for _, row in df.iterrows():
            image_name = row[image_column]
            full_path = os.path.join(image_folder, image_name)

            if not os.path.exists(full_path) or not any(image_name.lower().endswith(ext) for ext in ['.png', '.jpg', '.jpeg', '.tiff', '.JPG']):
                print(f"⚠️ Image not found or invalid: {full_path}")
                lai_list.append(None)
                gap_list.append([None] * len(indices))
                continue

            lai, gaps = calculate_lai_fixed_rings(full_path, clumping_index=clumping_index, ring_indices=indices)
            lai_list.append(lai)
            gap_list.append(gaps)

        df[f'LAI_{ring_count}rings'] = lai_list
        df[f'Gaps_{ring_count}rings'] = gap_list

    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    output_excel_path = excel_path.replace(".xlsx", f"_lai_kmens_555_rings.xlsx")
    df.to_excel(output_excel_path, index=False)
    print(f"✅ LAI data saved to {output_excel_path}")

    return df

#Execution function example, commented to avoid automatic execution here
process_lai_excel_fixed_rings(
    excel_path=r'../image_date_plot2.xlsx',
    image_column='ImageName',
    image_folder=r'./../../data/kmeans-mix-cropped-jpg',
    clumping_index=0.7,
)
