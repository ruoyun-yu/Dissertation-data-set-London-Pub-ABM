import requests
import time
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point
import re
import os
from tqdm import tqdm

API_KEY = 'AIzaSyBKwgmBO9hydDwa45xub63NTr1yUVH_lS4'  # ← 替换为你的 API Key
GPKG_PATH = 'london_borough_geopackage.gpkg'
OUTPUT_FILE = 'london_pubs_basic.csv'
TEMP_FILE = 'london_pubs_basic_temp.csv'

borough_pub_counts = {
    'Westminster': 445, 'Camden': 245, 'Islington': 210, 'Hackney': 210,
    'Southwark': 180, 'Lambeth': 160, 'City of London': 145,
    'Tower Hamlets': 150, 'Wandsworth': 125, 'Hammersmith and Fulham': 105,
    'Lewisham': 100, 'Kensington and Chelsea': 100, 'Bromley': 100,
    'Richmond upon Thames': 95, 'Bexley': 90, 'Croydon': 80, 'Ealing': 80,
    'Hillingdon': 80, 'Barnet': 75, 'Hounslow': 75, 'Enfield': 70,
    'Greenwich': 65, 'Haringey': 65, 'Brent': 65, 'Harrow': 45, 'Sutton': 45,
    'Redbridge': 45, 'Newham': 45, 'Merton': 50, 'Kingston upon Thames': 55,
    'Waltham Forest': 55, 'Barking and Dagenham': 20
}

density_settings = {
    'low':    {'spacing': 1000, 'radius': 700},
    'medium': {'spacing': 600,  'radius': 500},
    'high':   {'spacing': 300,  'radius': 400},
}

def classify_density(pub_count):
    if pub_count <= 70:
        return 'low'
    elif pub_count <= 150:
        return 'medium'
    else:
        return 'high'

def generate_grid(geom, spacing):
    minx, miny, maxx, maxy = geom.bounds
    points = []
    x = minx
    while x <= maxx:
        y = miny
        while y <= maxy:
            p = Point(x, y)
            if geom.contains(p):
                p_wgs84 = gpd.GeoSeries([p], crs='EPSG:27700').to_crs('EPSG:4326').geometry[0]
                points.append((p_wgs84.y, p_wgs84.x))
            y += spacing
        x += spacing
    return points

def extract_postcode(address):
    if not address:
        return None
    match = re.search(r'[A-Z]{1,2}\d{1,2}[A-Z]?\s?\d[A-Z]{2}', address)
    return match.group() if match else None

def search_nearby(lat, lng, radius, api_key):
    results = []
    base_url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
    params = {
        'location': f'{lat},{lng}',
        'radius': radius,
        'keyword': 'pub',
        'type': 'point_of_interest',
        'key': api_key
    }

    for _ in range(3):
        try:
            res = requests.get(base_url, params=params, timeout=10).json()
            if 'results' in res:
                results.extend(res['results'])
            if 'next_page_token' in res:
                time.sleep(2)
                params['pagetoken'] = res['next_page_token']
            else:
                break
        except Exception as e:
            print(f"⚠️ 请求失败或超时，跳过该点: {e}")
            break
    return results

# 加载已有数据
existing_df = pd.read_csv(OUTPUT_FILE) if os.path.exists(OUTPUT_FILE) else pd.DataFrame()
seen_ids = set(existing_df['place_id']) if not existing_df.empty else set()
seen_boroughs = set(existing_df['borough']) if not existing_df.empty else set()

gdf = gpd.read_file(GPKG_PATH, layer=0).to_crs(epsg=27700)
all_rows = []

for idx, row in gdf.iterrows():
    borough = row['NAME']
    if borough in seen_boroughs:
        print(f"⏭️ 跳过已完成 Borough: {borough}")
        continue

    pub_count = borough_pub_counts.get(borough, 50)
    density = classify_density(pub_count)
    setting = density_settings[density]

    print(f"\n📍 正在抓取 Borough: {borough}（密度等级：{density}）")
    grid_points = generate_grid(row['geometry'], setting['spacing'])
    print(f"  → 格网点数: {len(grid_points)}")

    for lat, lng in tqdm(grid_points, desc=f"{borough}"):
        try:
            places = search_nearby(lat, lng, setting['radius'], API_KEY)
            for place in places:
                pid = place.get('place_id')
                if not pid or pid in seen_ids:
                    continue

                loc = place.get('geometry', {}).get('location', {})
                address = place.get('vicinity')

                entry = {
                    'place_id': pid,
                    'name': place.get('name'),
                    'address': address,
                    'postcode': extract_postcode(address),
                    'lat': loc.get('lat'),
                    'lng': loc.get('lng'),
                    'rating': place.get('rating'),
                    'user_ratings_total': place.get('user_ratings_total'),
                    'borough': borough
                }
                all_rows.append(entry)
                seen_ids.add(pid)

                # 增量保存
                pd.DataFrame([entry]).to_csv(TEMP_FILE, mode='a', index=False, header=not os.path.exists(TEMP_FILE))

        except Exception as e:
            print(f"⚠️ 错误：{e}")

# 合并保存
df_new = pd.read_csv(TEMP_FILE) if os.path.exists(TEMP_FILE) else pd.DataFrame()
final_df = pd.concat([existing_df, df_new], ignore_index=True)
final_df.to_csv(OUTPUT_FILE, index=False)
if os.path.exists(TEMP_FILE):
    os.remove(TEMP_FILE)

print(f"\n✅ 抓取完成，共 {len(final_df)} 个 pub，结果保存至 {OUTPUT_FILE}")
