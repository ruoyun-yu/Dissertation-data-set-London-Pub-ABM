# Load necessary packages
library(sf)
library(dplyr)
library(readr)
library(ggplot2)

dir.create("/Users/amyyu/Documents/CASA/dissertation_pubs/data/spending_2021", recursive = TRUE, showWarnings = FALSE)

# Step 1: Read OA shapefile
oa_sf <- st_read("/Users/amyyu/Documents/CASA/dissertation_pubs/data/Sector_Output_Areas_2021.geojson")

# Extract borough name from LSOA name
oa_sf$borough_clean <- sub(" [0-9]+[A-Za-z]*$", "", oa_sf$LSOA21NM)

# Define list of Greater London boroughs
london_boroughs <- c(
  "Camden", "Islington", "Hackney", "Tower Hamlets", "Southwark",
  "Lambeth", "Wandsworth", "Westminster", "Kensington and Chelsea",
  "Hammersmith and Fulham", "Lewisham", "Greenwich", "Barking and Dagenham",
  "Barnet", "Bexley", "Brent", "Bromley", "Croydon", "Ealing", "Enfield",
  "Haringey", "Harrow", "Havering", "Hillingdon", "Hounslow", "Kingston upon Thames",
  "Merton", "Newham", "Redbridge", "Richmond upon Thames", "Sutton", "Waltham Forest",
  "City of London"
)

# Filter London areas
london_sf <- oa_sf %>%
  filter(borough_clean %in% london_boroughs)

# Save filtered London OA data
st_write(london_sf, "/Users/amyyu/Documents/CASA/dissertation_pubs/data/spending_2021/london_oa_2021.geojson", delete_layer = TRUE)

# Read postcode sector lookup table
postcode_lookup <- read_csv("/Users/amyyu/Documents/CASA/dissertation_pubs/data/Output_Area_to_Postcode_Sector_(May_2021)_Lookup_in_England_and_Wales.csv")

# Join postcode sector to OA polygons
london_sf <- london_sf %>%
  left_join(postcode_lookup, by = c("OA21CD" = "OA21CD"))

# Save joined data
st_write(london_sf, "/Users/amyyu/Documents/CASA/dissertation_pubs/data/spending_2021/london_oa_with_postcode.geojson", delete_layer = TRUE)

# Ensure valid geometries
london_sf <- london_sf %>% mutate(geometry = st_make_valid(geometry))

# Aggregate by postcode sector (PCDS)
london_sf_merged <- london_sf %>%
  group_by(PCDS) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

st_write(london_sf_merged, "/Users/amyyu/Documents/CASA/dissertation_pubs/data/spending_2021/london_postcode_sector.geojson", delete_layer = TRUE)

# Read the merged postcode sector geometry
merged_sf <- st_read("/Users/amyyu/Documents/CASA/dissertation_pubs/data/spending_2021/london_postcode_sector.geojson")

# Clean PCDS (remove spaces)
merged_sf <- merged_sf %>%
  mutate(PCDS_nospace = gsub(" ", "", PCDS))

# Load spending data
spending_df <- read_csv("/Users/amyyu/Documents/CASA/dissertation_pubs/data/spending_total_london.csv")

# Join spending data to geometry
merged_sf <- merged_sf %>%
  left_join(spending_df, by = c("PCDS_nospace" = "Postal Code"))

# Create adjacency list for spatial filling
neighbors_list <- st_touches(merged_sf)

# List of all year columns
year_cols <- grep("^\\d{4}$", names(merged_sf), value = TRUE)

# Copy for filling
merged_sf_spatial_fill <- merged_sf

# Spatial imputation by neighbor average
for (year in year_cols) {
  for (i in seq_len(nrow(merged_sf_spatial_fill))) {
    if (is.na(merged_sf_spatial_fill[[year]][i])) {
      neighbor_ids <- neighbors_list[[i]]
      if (length(neighbor_ids) > 0) {
        neighbor_vals <- merged_sf_spatial_fill[[year]][neighbor_ids]
        neighbor_mean <- mean(neighbor_vals, na.rm = TRUE)
        if (!is.nan(neighbor_mean)) {
          merged_sf_spatial_fill[[year]][i] <- neighbor_mean
        }
      }
    }
  }
}

# Visualize filled 2021 spending map
ggplot(merged_sf_spatial_fill) +
  geom_sf(aes(fill = `2021`)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(title = "London Postcode Sector Total Spending (2021)", fill = "Spending £")

# Create comparison dataset for before/after filling
df1 <- merged_sf %>% mutate(source = "Original") %>% select(geometry, value = `2021`, source)
df2 <- merged_sf_spatial_fill %>% mutate(source = "Filled") %>% select(geometry, value = `2021`, source)
compare_sf <- rbind(df1, df2)
compare_sf$source <- factor(compare_sf$source, levels = c("Original", "Filled"))

# Side-by-side comparison map
ggplot(compare_sf) +
  geom_sf(aes(fill = value)) +
  facet_wrap(~source) +
  scale_fill_viridis_c(option = "plasma", limits = c(0, 17), na.value = "grey90") +
  theme_minimal() +
  labs(title = "London 2021 Spending: Original vs Filled", fill = "Spending £")

# 假设文件保存为 CSV，路径如下（你根据实际改）
pop_df <- read_csv("/Users/amyyu/Documents/CASA/dissertation_pubs/data/oa_population_2021.csv")

# 选择并重命名需要的列
pop_df <- pop_df %>%
  select(OA21CD = `OA 2021 Code`, population = Total)

# 合并人口数据进 OA 空间数据（london_sf）
london_sf <- london_sf %>%
  left_join(pop_df, by = "OA21CD")

sector_pop <- london_sf %>%
  group_by(PCDS) %>%
  summarise(sector_population = sum(population, na.rm = TRUE), .groups = "drop")

# 合并回 OA 数据
london_sf <- london_sf %>%
  left_join(st_drop_geometry(sector_pop), by = "PCDS")

# 从填补后的 merged_sf_spatial_fill 提取支出列 + PCDS_nospace
filled_spending <- merged_sf_spatial_fill %>%
  st_drop_geometry() %>%
  select(PCDS_nospace, all_of(year_cols))%>%
  distinct(PCDS_nospace, .keep_all = TRUE)  # year_cols = "2021", "2020", ...

# 合并填补后的支出数据到 OA
london_sf <- london_sf %>%
  mutate(PCDS_nospace = gsub(" ", "", PCDS)) %>%
  left_join(filled_spending, by = "PCDS_nospace")

# 计算2021年加权支出
london_sf <- london_sf %>%
  mutate(spending_2021_oa = (`2021`) * (population / sector_population))

ggplot(london_sf) +
  geom_sf(aes(fill = spending_2021_oa)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(title = "OA-level Weighted Spending (2021)", fill = "Spending £")
# 创建导出目录（如果还没创建）
dir.create("/Users/amyyu/Documents/CASA/dissertation_pubs/data/spending_2021/london_oa_shapefile", showWarnings = FALSE)

# 导出为 shapefile
st_write(
  london_sf,
  "/Users/amyyu/Documents/CASA/dissertation_pubs/data/spending_2021/london_oa_shapefile/london_oa_spending.shp",
  delete_layer = TRUE
)

# 指定导出路径
output_path <- "/Users/amyyu/Documents/CASA/dissertation_pubs/data/spending_2021/london_oa_shapefile/london_oa_spending_with_weighted.shp"

# 若只保留核心字段，可精简选择以下字段：
london_sf_export <- london_sf %>%
  select(
    OA21CD,               # Output Area Code
    borough_clean,        # Borough Name
    PCDS,                 # Postcode Sector
    population,           # OA 人口
    sector_population,    # 所属 PCDS 总人口
    `2021`,               # 填补后的 Sector 支出
    spending_2021_oa,     # 加权支出
    geometry
  ) %>%
  filter(!st_is_empty(geometry))  # ⬅️ 移除空 geometry，防止重复记录

# 导出为 shapefile
st_write(
  london_sf_export,
  output_path,
  delete_layer = TRUE
)
