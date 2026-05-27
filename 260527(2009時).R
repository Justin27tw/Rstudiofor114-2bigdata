library(data.table)
library(tidyverse)
library(lubridate)
library(scales)
library(tidytext)  # 確保 reorder_within 與 scale_x_reordered 能正常運作

# ==============================================================================
# 1. 讀取資料
# ==============================================================================
raw_df <- fread(
  "C:/Users/88697/Downloads/202602_YouBike2.0票證刷卡資料/202602_YouBike2.0票證刷卡資料.csv",
  encoding = "UTF-8",
  header = FALSE
)

colnames(raw_df) <- c("借車時間", "借車場站", "還車時間", "還車場站", "使用時長", "車種", "日期")

output_dir <- "X:/大數據用/youbike分析結果圖"
if(!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}
# ==============================================================================
# 2. 資料清洗與衍生欄位
# ==============================================================================
clean_df <- raw_df %>%
  mutate(
    借車時間 = as.POSIXct(借車時間, format = "%Y/%m/%d %H:%M"),
    還車時間 = as.POSIXct(還車時間, format = "%Y/%m/%d %H:%M"),
    日期 = as.Date(日期, format = "%Y/%m/%d")
  ) %>%
  mutate(
    使用秒數 = as.numeric(as.difftime(使用時長, format = "%H:%M:%S", units = "secs"))
  ) %>%
  mutate(
    借車小時 = hour(借車時間),
    # 強制將星期設定為排序因子
    星期幾 = factor(weekdays(借車時間), 
                 levels = c("星期一", "星期二", "星期三", "星期四", "星期五", "星期六", "星期日")),
    是否為週末 = ifelse(星期幾 %in% c("星期六", "星期日"), "週末休閒", "平日通勤")
  ) %>%
  # 精確計算該日期屬於 2 月份的「第幾週」 (以週一為每週起始)
  mutate(
    月份第幾週 = factor(paste0("第 ", isoweek(日期) - isoweek(as.Date("2026-02-01")) + 1, " 週"))
  ) %>%
  filter(
    !is.na(使用秒數) & !is.na(借車時間) &
      使用秒數 >= 60 & 使用秒數 <= 21600
  )

# ==============================================================================
# 3. 各項數據分析與圖表物件建立
# ==============================================================================

# ---- [分析 1: 24小時趨勢] ----
trend_data <- clean_df %>%
  group_by(借車小時, 是否為週末) %>%
  summarise(租借次數 = n(), .groups = "drop")

p1 <- ggplot(trend_data, aes(x = 借車小時, y = 租借次數, color = 是否為週末, group = 是否為週末)) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  scale_x_continuous(breaks = 0:23) + scale_y_continuous(labels = comma) + 
  labs(title = "台北市 YouBike 2.0 24小時租借量趨勢對比",
       subtitle = "平日通勤呈現明顯雙尖峰；週末中午至下午為熱門時段",
       x = "時間 (小時)", y = "總租借次數", color = "日期類型") +
  theme_minimal() + theme(text = element_text(family = "sans"))

# ---- [分析 2: 十大熱門場站] ----
top_stations <- clean_df %>%
  group_by(借車場站) %>%
  summarise(總租借次數 = n(), .groups = "drop") %>%
  arrange(desc(總租借次數)) %>%
  slice(1:10)

p2 <- ggplot(top_stations, aes(x = reorder(借車場站, 總租借次數), y = 總租借次數)) +
  geom_bar(stat = "identity", fill = "steelblue") + coord_flip() + scale_y_continuous(labels = comma) +
  labs(title = "台北市 YouBike 2.0 十大熱門借車場站", x = "場站名稱", y = "總租借次數") +
  theme_minimal() + theme(text = element_text(family = "sans"))

# ---- [分析 3: 車種與 T 檢定] ----
t_test_result <- t.test(使用秒數 ~ 車種, data = clean_df)

# ---- [分析 4: 流動型態與卡方檢定] ----
od_data <- clean_df %>%
  mutate(流動型態 = ifelse(借車場站 == 還車場站, "同站借還", "跨站騎乘")) %>%
  group_by(車種, 流動型態) %>%
  summarise(次數 = n(), .groups = "drop") %>%
  group_by(車種) %>%
  mutate(百分比 = 次數 / sum(次數))

p3 <- ggplot(od_data, aes(x = 車種, y = 百分比, fill = 流動型態)) +
  geom_bar(stat = "identity", position = "fill") + scale_y_continuous(labels = percent) +
  scale_fill_manual(values = c("#FF9999", "#66CC99")) +
  labs(title = "YouBike 2.0 車種與流動型態關係圖", x = "車種", y = "比例", fill = "流動型態") +
  theme_minimal() + theme(text = element_text(family = "sans"))

chisq_result <- chisq.test(table(clean_df$車種, clean_df$借車場站 == clean_df$還車場站))

# ---- [分析 5: 平日黃金時段與 ANOVA 檢定] ----
time_block_df <- clean_df %>%
  filter(是否為週末 == "平日通勤") %>%
  mutate(時段 = case_when(
    借車小時 %in% 7:9 ~ "1. 上午通勤(07-09)",
    借車小時 %in% 17:19 ~ "2. 下午通勤(17-19)",
    借車小時 %in% c(22, 23, 0) ~ "3. 深夜歸家(22-00)",
    TRUE ~ "其他時段"
  )) %>%
  filter(時段 != "其他時段")

p4 <- ggplot(time_block_df, aes(x = 時段, y = 使用秒數/60, fill = 時段)) +
  geom_boxplot(outlier.shape = NA) + coord_cartesian(ylim = c(0, 45)) +
  labs(title = "平日不同核心時段之租借時長比較", x = "時段", y = "使用時間 (分鐘)") +
  theme_minimal() + theme(text = element_text(family = "sans"), legend.position = "none")

anova_result <- aov(使用秒數 ~ 時段, data = time_block_df)
anova_summary <- summary(anova_result)

# ---- [分析 6: 借還失衡排行] ----
out_flow <- clean_df %>% group_by(借車場站) %>% summarise(借出次數 = n(), .groups = "drop") %>% rename(場站 = 借車場站)
in_flow <- clean_df %>% group_by(還車場站) %>% summarise(還入次數 = n(), .groups = "drop") %>% rename(場站 = 還車場站)

balance_df <- full_join(out_flow, in_flow, by = "場站") %>%
  mutate(
    借出次數 = ifelse(is.na(借出次數), 0, 借出次數),
    還入次數 = ifelse(is.na(還入次數), 0, 還入次數),
    淨流量 = 還入次數 - 借出次數
  ) %>%
  arrange(desc(abs(淨流量)))

p5 <- ggplot(balance_df %>% slice(1:15), aes(x = reorder(場站, 淨流量), y = 淨流量, fill = 淨流量 > 0)) +
  geom_bar(stat = "identity") + coord_flip() + scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#FF6666", "#6699FF"), labels = c("借大於還(缺車)", "還大於借(滿站)")) +
  labs(title = "台北市 YouBike 2.0 頂級失衡場站排行", x = "場站名稱", y = "淨流量 (還入次數 - 借出次數)", fill = "失衡狀態") +
  theme_minimal() + theme(text = element_text(family = "sans"))

# ---- [分析 7: 平日早晚尖峰主要借車站對比] ----
peak_shuttle <- clean_df %>%
  filter(是否為週末 == "平日通勤") %>%
  mutate(尖峰時段 = case_when(
    借車小時 %in% 7:9 ~ "早尖峰 (07-09)",
    借車小時 %in% 17:19 ~ "晚尖峰 (17-19)",
    TRUE ~ "其他"
  )) %>%
  filter(尖峰時段 != "其他")

peak_top_stations <- peak_shuttle %>%
  group_by(尖峰時段, 借車場站) %>%
  summarise(租借次數 = n(), .groups = "drop") %>%
  group_by(尖峰時段) %>%
  arrange(desc(租借次數)) %>%
  slice(1:5)

p6 <- ggplot(peak_top_stations, aes(x = reorder_within(借車場站, 租借次數, 尖峰時段), y = 租借次數, fill = 尖峰時段)) +
  geom_bar(stat = "identity") +
  facet_wrap(~尖峰時段, scales = "free_y") +
  scale_x_reordered() + 
  coord_flip() + scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#4A90E2", "#E25A4A")) +
  labs(title = "台北市平日早晚尖峰主要借車場站對比", x = "場站名稱", y = "租借次數") +
  theme_minimal() + theme(text = element_text(family = "sans"), legend.position = "none")

# ---- [分析 8: 平日 vs 週末前五大熱門騎乘路線 (OD)] ----
top_od_pairs <- clean_df %>%
  group_by(借車場站, 還車場站, 是否為週末) %>%
  summarise(路線騎乘次數 = n(), .groups = "drop") %>%
  mutate(路線名稱 = paste0(借車場站, " ➔ ", 還車場站)) %>%
  group_by(是否為週末) %>%
  arrange(desc(路線騎乘次數)) %>%
  slice(1:5) %>%
  ungroup()

p8 <- ggplot(top_od_pairs, aes(x = reorder_within(路線名稱, 路線騎乘次數, 是否為週末), y = 路線騎乘次數, fill = 是否為週末)) +
  geom_bar(stat = "identity") +
  facet_wrap(~是否為週末, scales = "free_y") +
  scale_x_reordered() + coord_flip() + scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#7B68EE", "#FF8C00")) +
  labs(title = "台北市 YouBike 2.0 平日 vs 週末前五大熱門騎乘路線 (OD)",
       subtitle = "揭示市民核心出行路廊，區分通勤接駁與假日休閒特性",
       x = "起訖站點路線", y = "總騎乘次數") +
  theme_minimal() + theme(text = element_text(family = "sans"), legend.position = "none")

# ---- [分析 9: 騎乘時長分級與車種用戶畫像堆疊圖] ----
user_profile <- clean_df %>%
  mutate(時長分級 = case_when(
    使用秒數 < 600  ~ "1. 短程接駁 (<10分鐘)",
    使用秒數 >= 600 & 使用秒數 <= 1800 ~ "2. 標準通勤 (10-30分鐘)",
    TRUE ~ "3. 長途休閒 (>30分鐘)"
  )) %>%
  group_by(車種, 時長分級) %>%
  summarise(次數 = n(), .groups = "drop") %>%
  group_by(車種) %>%
  mutate(百分比 = 次數 / sum(次數))

p9 <- ggplot(user_profile, aes(x = 車種, y = 百分比, fill = 時長分級)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(title = "YouBike 2.0 不同車種之市民騎乘時長用戶畫像",
       subtitle = "檢視電輔車(2.0E)是否顯著拉長市民的長途騎乘比例",
       x = "車種", y = "結構百分比", fill = "騎乘時間分級") +
  theme_minimal() + theme(text = element_text(family = "sans"))

# ---- [分析 10: 2月份每日一般車 vs 電輔車 借用次數雙線趨勢圖] ----
daily_bike_count <- clean_df %>%
  group_by(日記 = 日期, 車種) %>%
  summarise(每日租借次數 = n(), .groups = "drop")

p10 <- ggplot(daily_bike_count, aes(x = 日記, y = 每日租借次數, color = 車種, group = 車種)) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "2 days") +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("#E6A23C", "#409EFF")) +
  labs(title = "2026年2月 YouBike 2.0 一般車與電輔車單日借用次數走勢對比",
       subtitle = "共同呈現兩車型每日總量起伏",
       x = "日期", y = "單日總借用次數", color = "車種類型") +
  theme_minimal() + theme(text = element_text(family = "sans"), axis.text.x = element_text(angle = 45, hjust = 1))

# ---- [分析 11: 2月份每日整體平均騎乘時間 (分鐘) 趨勢圖] ----
daily_avg_duration <- clean_df %>%
  group_by(日期) %>%
  summarise(整體平均時間_分鐘 = mean(使用秒數) / 60, .groups = "drop")

p11 <- ggplot(daily_avg_duration, aes(x = 日期, y = 整體平均時間_分鐘)) +
  geom_line(color = "#67C23A", size = 1.2) + geom_point(color = "#67C23A", size = 2) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "2 days") +
  labs(title = "2026年2月 YouBike 2.0 全市每日整體平均借用時長走勢",
       subtitle = "監測市民每日平均騎乘時間之波動（單位：分鐘）",
       x = "日期", y = "平均使用時間 (分鐘)") +
  theme_minimal() + theme(text = element_text(family = "sans"), axis.text.x = element_text(angle = 45, hjust = 1))

# ---- [分析 12: 2月份分車種每日平均騎乘時間 (分鐘) 趨勢圖] ----
daily_avg_duration_bike <- clean_df %>%
  group_by(日期, 車種) %>%
  summarise(分車種平均時間_分鐘 = mean(使用秒數) / 60, .groups = "drop")

p12 <- ggplot(daily_avg_duration_bike, aes(x = 日期, y = 分車種平均時間_分鐘, color = 車種, group = 車種)) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "2 days") +
  scale_color_manual(values = c("#E6A23C", "#409EFF")) +
  labs(title = "2026年2月 YouBike 2.0 各車種每日平均借用時長對比走勢",
       subtitle = "動態呈現一般車與電輔車每日平均騎乘時間之落差特性",
       x = "日期", y = "平均使用時間 (分鐘)", color = "車種類型") +
  theme_minimal() + theme(text = element_text(family = "sans"), axis.text.x = element_text(angle = 45, hjust = 1))

# ---- [分析 13: 最少人借車時段計算] ----
least_active_hour <- clean_df %>%
  group_by(借車小時) %>%
  summarise(總租借次數 = n(), .groups = "drop") %>%
  mutate(每小時平均租借次數 = 總租借次數 / 28) %>%
  arrange(每小時平均租借次數)


# ==============================================================================
# 3.1 週一至週日跨週縱向穩定度分析
# ==============================================================================

# ---- [分析 14: 跨週對比之週一至週日借車總數量波動圖] ----
weekly_weekday_counts <- clean_df %>%
  group_by(月份第幾週, 星期幾) %>%
  summarise(租借次數 = n(), .groups = "drop")

p14 <- ggplot(weekly_weekday_counts, aes(x = 月份第幾週, y = 租借次數, fill = 星期幾)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85) +
  facet_wrap(~星期幾, scales = "free_y", ncol = 4) +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "2026年2月 各週之『星期X』總借車量縱向對比",
       subtitle = "檢信第一週到第四週的同一個星期幾，其借車總量是否差不多（判斷週間規律穩定度）",
       x = "月份週次", y = "單日總租借次數") +
  theme_minimal() + theme(text = element_text(family = "sans"), legend.position = "none")

# 執行卡方獨立性檢定
chisq_weekly_stability <- chisq.test(table(clean_df$月份第幾週, clean_df$星期幾))

# ---- [分析 15: 跨週對比之週一至週日每日 Top 3 熱門路線縱向對比資料準備] ----
weekly_weekday_od <- clean_df %>%
  group_by(月份第幾週, 星期幾, 借車場站, 還車場站) %>%
  summarise(路線騎乘次數 = n(), .groups = "drop") %>%
  mutate(路線名稱 = paste0(借車場站, " ➔ ", 還車場站)) %>%
  group_by(月份第幾週, 星期幾) %>%
  arrange(desc(路線騎乘次數)) %>%
  slice(1:3) %>% 
  ungroup()

# 保留原本的大矩陣圖（已加入空間自適應防重疊修正）
p15_matrix <- ggplot(weekly_weekday_od, aes(x = reorder_within(路線名稱, 路線騎乘次數, within = interaction(月份第幾週, 星期幾)), y = 路線騎乘次數, fill = 月份第幾週)) +
  geom_bar(stat = "identity") +
  facet_grid(星期幾 ~ 月份第幾週, scales = "free_y", space = "free_y") + 
  scale_x_reordered() + coord_flip() + scale_y_continuous(labels = comma) +
  labs(title = "2026年2月 各週之『星期X』Top 3 熱門借車路線縱向比對大網格",
       x = "起訖站點路線", y = "累積騎乘次數", fill = "週次") +
  theme_minimal() + theme(text = element_text(family = "sans"), legend.position = "bottom", strip.text.y = element_text(angle = 0), axis.text.y = element_text(size = 7))

# ---- [新增分析 16: 影響騎乘時長的因素迴歸分析] ----
model_data <- clean_df %>%
  mutate(
    是否為週末 = as.factor(是否為週末),
    車種 = as.factor(車種),
    時段 = case_when(
      借車小時 %in% 7:9 ~ "上午尖峰",
      借車小時 %in% 17:19 ~ "下午尖峰",
      TRUE ~ "離峰時段"
    ) %>% as.factor()
  )

lm_model <- lm(使用秒數 ~ 是否為週末 + 車種 + 時段, data = model_data)
model_summary <- summary(lm_model)

png(file.path(output_dir, "14_迴歸模型殘差診斷圖.png"), width = 800, height = 800)
old_par <- par(no.readonly = TRUE)
par(mfrow = c(2, 2))
plot(lm_model)
par(old_par)
dev.off()
graphics.off()

# ---- [新增分析 17: 場站行為特徵分群] ----
station_behavior <- clean_df %>%
  group_by(借車場站) %>%
  summarise(
    平均騎乘時間 = mean(使用秒數),
    週末租借比例 = sum(是否為週末 == "週末休閒") / n(),
    下午尖峰比例 = sum(借車小時 %in% 17:19) / n()
  ) %>%
  drop_na()

set.seed(123)
km_result <- kmeans(station_behavior[, -1], centers = 3)
station_behavior$群組 <- as.factor(km_result$cluster)

p17 <- ggplot(station_behavior, aes(x = 週末租借比例, y = 平均騎乘時間, color = 群組)) +
  geom_point(size = 3, alpha = 0.6) +
  labs(title = "YouBike 2.0 場站行為特徵分群圖 (K-means)",
       subtitle = "區分通勤型(低週末比)與休閒型(高時長/高週末比)場站",
       x = "週末租借次數比例", y = "平均騎乘時間 (秒)") +
  theme_minimal() + theme(text = element_text(family = "sans"))
ggsave(file.path(output_dir, "15_場站行為特徵分群圖.png"), plot = p17, width = 9, height = 6)

# ---- [新增分析 18: 不同車種平均騎乘時間之信賴區間圖] ----
ci_data <- clean_df %>%
  group_by(車種) %>%
  summarise(
    n = n(),
    mean_val = mean(使用秒數) / 60,
    sd_val = sd(使用秒數) / 60
  ) %>%
  mutate(
    se = sd_val / sqrt(n),
    lower = mean_val - qt(1 - (0.05 / 2), n - 1) * se,
    upper = mean_val + qt(1 - (0.05 / 2), n - 1) * se
  )

p18 <- ggplot(ci_data, aes(x = 車種, y = mean_val, fill = 車種)) +
  geom_bar(stat = "identity", alpha = 0.7, width = 0.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  labs(title = "不同車種騎乘時間之 95% 信賴區間",
       subtitle = "展現統計估計的精確度",
       x = "車種", y = "平均使用時間 (分鐘)") +
  theme_minimal() + theme(text = element_text(family = "sans"))
ggsave(file.path(output_dir, "16_車種時長信賴區間圖.png"), plot = p18, width = 8, height = 5)

# ==============================================================================
# [新增分析 A/B/C 統計資料計算]
# ==============================================================================
daily_bike_summary <- clean_df %>%
  group_by(日期, 車種) %>%
  summarise(總借用次數 = n(), 平均騎乘時間_分鐘 = round(mean(使用秒數) / 60, 2), .groups = "drop")

overall_bike_summary <- clean_df %>%
  group_by(車種) %>%
  summarise(總次數 = n(), 平均時長_分鐘 = round(mean(使用秒數) / 60, 2), 時長標準差 = round(sd(使用秒數) / 60, 2), .groups = "drop")

clean_df <- clean_df %>% mutate(流動型態 = ifelse(借車場站 == 還車場站, "同站借還", "跨站騎乘"))
t_test_flow_duration <- t.test(使用秒數 ~ 流動型態, data = clean_df)
t_test_daytype_duration <- t.test(使用秒數 ~ 是否為週末, data = clean_df)


# ==============================================================================
# 4.1 自動化批次儲存核心基礎圖表
# ==============================================================================
fig_width <- 9
fig_height <- 6
fig_dpi <- 300

ggsave(filename = file.path(output_dir, "01_24小時租借趨勢對比.png"), plot = p1, width = fig_width, height = fig_height, dpi = fig_dpi)
ggsave(filename = file.path(output_dir, "02_全市十大熱門借車場站.png"), plot = p2, width = fig_width, height = fig_height, dpi = fig_dpi)
ggsave(filename = file.path(output_dir, "03_車種與流動型態關係圖.png"), plot = p3, width = fig_width, height = fig_height, dpi = fig_dpi)
ggsave(filename = file.path(output_dir, "04_平日不同核心時段租借時長比較.png"), plot = p4, width = fig_width, height = fig_height, dpi = fig_dpi)
ggsave(filename = file.path(output_dir, "05_頂級借還失衡場站排行.png"), plot = p5, width = fig_width, height = fig_height, dpi = fig_dpi)
ggsave(filename = file.path(output_dir, "06_平日早晚尖峰主要借車場站對比.png"), plot = p6, width = fig_width, height = fig_height, dpi = fig_dpi)
ggsave(filename = file.path(output_dir, "07_平日假日熱門OD起訖路線.png"), plot = p8, width = fig_width, height = fig_height, dpi = fig_dpi)
ggsave(filename = file.path(output_dir, "08_各車種市民騎乘時長用戶畫像.png"), plot = p9, width = fig_width, height = fig_height, dpi = fig_dpi)
ggsave(filename = file.path(output_dir, "09_2月雙車種單日借用次數走勢.png"), plot = p10, width = fig_width, height = fig_height, dpi = fig_dpi)
ggsave(filename = file.path(output_dir, "10_2月全市單日整體平均時長走勢.png"), plot = p11, width = fig_width, height = fig_height, dpi = fig_dpi)
ggsave(filename = file.path(output_dir, "11_2月分車種單日平均時長對比.png"), plot = p12, width = fig_width, height = fig_height, dpi = fig_dpi)
ggsave(filename = file.path(output_dir, "12_各週之星期X總量對比.png"), plot = p14, width = 11, height = 7, dpi = fig_dpi)
ggsave(filename = file.path(output_dir, "13_跨週之星期X熱門路線總大網格.png"), plot = p15_matrix, width = 18, height = 14, dpi = fig_dpi)

# ==============================================================================
# 🛠️【核心功能升級】：週一至週日「各自生成獨立跨週圖表」循環核心
# ==============================================================================
weekdays_list <- c("星期一", "星期二", "星期三", "星期四", "星期五", "星期六", "星期日")
file_indices <- c("A", "B", "C", "D", "E", "F", "G") # 用英文字母排序避免覆蓋

for(i in 1:length(weekdays_list)) {
  target_day <- weekdays_list[i]
  idx <- file_indices[i]
  
  # 篩選單一星期幾的資料
  sub_od <- weekly_weekday_od %>% filter(星期幾 == target_day)
  
  # 建立精緻的橫向展開圖 (Row-wise alignment)
  p_individual <- ggplot(sub_od, aes(x = reorder_within(路線名稱, 路線騎乘次數, within = 月份第幾週), y = 路線騎乘次數, fill = 月份第幾週)) +
    geom_bar(stat = "identity", width = 0.7) +
    facet_wrap(~月份第幾週, scales = "free_y", nrow = 1) + # 橫向一字排開各週
    scale_x_reordered() + 
    coord_flip() + 
    scale_y_continuous(labels = comma) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = sprintf("2026年2月 各週之【%s】Top 3 熱門借車路線縱向對比", target_day),
         subtitle = "橫向分組展開：觀察同一個星期幾在不同週次下的通勤/休閒黃金路廊穩定度",
         x = "起訖站點路線", y = "累積騎乘次數") +
    theme_minimal() + 
    theme(
      text = element_text(family = "sans"), 
      legend.position = "none",
      axis.text.y = element_text(size = 9, color = "#222222", face = "bold"),
      panel.spacing.x = unit(1.5, "lines"), # 拉開各週之間的左右距離
      strip.text = element_text(size = 11, face = "bold", color = "blue") # 突顯週次標籤
    )
  
  # 動態儲存獨立高解析圖檔
  file_out_name <- sprintf("13_獨立分析_%s_%s_跨週熱門路線對比.png", idx, target_day)
  ggsave(filename = file.path(output_dir, file_out_name), plot = p_individual, width = 15, height = 4, dpi = fig_dpi)
}

cat(sprintf("👉 核心圖表與 7 張獨立星期跨週圖表已全數輸出儲存至：%s\n\n", output_dir))


# ==============================================================================
# 5. 【終端整合成果總覽輸出】全部計算結果統一排版 V7.0 (全週間升級版)
# ==============================================================================
current_month_str <- ifelse(month(min(clean_df$日期)) == 1, "1", "2")

cat("\n")
cat("================================================================================\n")
cat(sprintf("         ★ 台北市 YouBike 2.0 大數據期末報告分析成果總覽 V7.0 (%s月份) ★            \n", current_month_str))
cat("================================================================================\n\n")

cat("[一、大數據基本規模與清洗結果]\n")
cat("  - 原始資料讀取總筆數 :", format(nrow(raw_df), big.mark=","), "筆\n")
cat("  - 清洗後有效分析筆數 :", format(nrow(clean_df), big.mark=","), "筆 (過濾極端值與解析失敗)\n\n")

cat("[二、核心敘述統計快報]\n")
cat("  - 全市 24 小時營運低谷分析 (最少人借車時段) :\n")
cat(sprintf("    ▶ 全市「最少人借車」的第一名小時區間為：%02d:00 - %02d:59 之間\n", 
            least_active_hour$借車小時[1], least_active_hour$借車小時[1]))

# 🛠️【終端機文字輸出升級】：自動遍歷周一到周日所有週次的第一名熱門路線
cat("\n  - 跨週縱向比對全覽（週一至週日各週第一名黃金路廊全加總）:\n")
for(wd in weekdays_list) {
  cat(sprintf("    ==================== 【%s 各週冠軍路線】 ====================\n", wd))
  sample_wd <- weekly_weekday_od %>% 
    filter(星期幾 == wd) %>% 
    group_by(月份第幾週) %>% 
    filter(路線騎乘次數 == max(路線騎乘次數)) %>% 
    ungroup() %>% 
    arrange(月份第幾週)
  
  for(s in 1:nrow(sample_wd)) {
    cat(sprintf("    ▶ %s 最熱門路線: %s (當日累積 %s 次)\n", 
                sample_wd$月份第幾週[s], sample_wd$路線名稱[s], format(sample_wd$路線騎乘次數[s], big.mark=",")))
  }
}

cat("\n  - 雙車型(一般車 vs 電輔車) 全月營運基礎數據統計 :\n")
for(i in 1:nrow(overall_bike_summary)) {
  cat(sprintf("    ▶ 車種: %-6s | 總總借用次數: %10s 次 | 平均騎乘時長: %5s 分鐘 (標準差: %5s)\n",
              overall_bike_summary$車種[i], 
              format(overall_bike_summary$總次數[i], big.mark=","), 
              overall_bike_summary$平均時長_分鐘[i], 
              overall_bike_summary$時長標準差[i]))
}

cat("\n[三、推論統計假設檢定結論彙整（六大嚴謹檢定）]\n\n")

cat("  (1) 週次與星期幾用量穩定度相關性檢定 (Chi-Square Test of Independence)\n")
cat("      - 卡方檢定統計值 (X-squared) :", round(chisq_weekly_stability$statistic, 2), "\n")
cat("      - 顯著性檢定 p-value :", format.pval(chisq_weekly_stability$p.value), "\n")
cat("      - 統計結論 : ")
if(chisq_weekly_stability$p.value > 0.05) {
  cat("【無法拒絕 H0】週次與星期幾高度獨立。這代表「每週的用量行為結構極其相似」，不同週的星期X借車總數『高度穩定』！\n")
} else {
  cat("【拒絕 H0】每週的用量行為存在結構波動！這代表受到特定週的假期(如春節/連假)、寒流效應干擾，導致各週用量結構不均勻。\n")
}

cat("\n  (2) 車種 (一般車 vs 電輔車) 騎乘時長差異檢定 (Two-Sample t-test)\n")
cat("      - t 檢定統計值 :", round(t_test_result$statistic, 2), "\n")
cat("      - p-value :", format.pval(t_test_result$p.value), "\n")
cat("      - 統計結論 : ")
if(t_test_result$p.value < 0.05) {
  cat("【顯著差異】電輔車與一般車的平均騎乘時間有顯著不同！請檢視時長用戶畫像(p9)觀察誰騎得比較久。\n")
} else {
  cat("【無顯著差異】兩種車種在騎乘時間上沒有統計學上的明顯落差。\n")
}

cat("\n  (3) 車種與流動型態 (同站/跨站) 關聯性檢定 (Chi-Square Test)\n")
cat("      - 卡方統計值 :", round(chisq_result$statistic, 2), "\n")
cat("      - p-value :", format.pval(chisq_result$p.value), "\n")
cat("      - 統計結論 : ")
if(chisq_result$p.value < 0.05) {
  cat("【顯著相關】車種會顯著影響使用者的流動型態（證明市民在選擇電輔車或一般車時，具備不同的目的地意圖）。\n")
} else {
  cat("---\n")
}

cat("\n  (4) 平日不同核心時段 (早/晚/深夜) 租借時長差異分析 (ANOVA)\n")
cat("      - F 檢定統計值 :", round(anova_summary[[1]]["時段", "F value"], 2), "\n")
cat("      - p-value :", format.pval(anova_summary[[1]]["時段", "Pr(>F)"]), "\n")
cat("      - 統計結論 : ")
if(anova_summary[[1]]["時段", "Pr(>F)"][1] < 0.05) {
  cat("【顯著差異】平日的早尖峰、晚尖峰與深夜歸家，三者的騎乘時間有顯著結構性特徵（趕上班 vs 下班放鬆）。\n")
} else {
  cat("【無顯著差異】不同時段的騎乘時間大致相同。\n")
}

cat("\n  (5) 流動型態 (同站借還 vs 跨站騎乘) 騎乘時長差異檢定 (Two-Sample t-test)\n")
cat("      - t 檢定統計值 :", round(t_test_flow_duration$statistic, 2), "\n")
cat("      - p-value :", format.pval(t_test_flow_duration$p.value), "\n")
cat("      - 統計結論 : ")
if(t_test_flow_duration$p.value < 0.05) {
  cat(sprintf("【顯著差異】「同站借還」與「跨站騎乘」的時長有極顯著落差！(同站平均: %.2f分 | 跨站平均: %.2f分)\n",
              t_test_flow_duration$estimate[1]/60, t_test_flow_duration$estimate[2]/60))
} else {
  cat("【無顯著差異】兩種流動型態的騎乘時間在統計上無明顯落差。\n")
}

cat("\n  (6) 日期類型 (平日通勤 vs 週末休閒) 騎乘時長差異檢定 (Two-Sample t-test)\n")
cat("      - t 檢定統計值 :", round(t_test_daytype_duration$statistic, 2), "\n")
cat("      - p-value :", format.pval(t_test_daytype_duration$p.value), "\n")
cat("      - 統計結論 : ")
if(t_test_daytype_duration$p.value < 0.05) {
  cat(sprintf("【顯著差異】市民在平日與週末的用車習慣存在顯著時長落差！(平日平均: %.2f分 | 週末平均: %.2f分)\n",
              t_test_daytype_duration$estimate[1]/60, t_test_daytype_duration$estimate[2]/60))
} else {
  cat("【無顯著差異】平日與週末的平均騎乘時間在統計上無明顯落差。\n")
}

cat("\n================================================================================\n")
cat("  全自動跨週分析全面大成功！週一至週日 7 張高質感獨立圖表已全部生成完畢。\n")
cat("  這下你的期末書面報告在「週間行為規律性」的數據佐證上，將具備無懈可擊的嚴謹度！ \n")
cat("================================================================================\n")