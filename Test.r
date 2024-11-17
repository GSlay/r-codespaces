# Đọc dữ liệu
GPU_data <- read.csv("All_GPUs.csv")
head(GPU_data,10)

# Xem tổng quan dữ liệu
library('dplyr')
data <- GPU_data %>% select('Release_Price','Core_Speed','Manufacturer','Memory','Memory_Bandwidth','Memory_Speed','Max_Power')
str(data)

# Loại bỏ ký tự thừa trong các cột
data$Release_Price <- gsub("\\$", "", data$Release_Price)
data$Core_Speed <- gsub("\\MHz", "", data$Core_Speed)
data$Memory <- gsub("\\MB", "", data$Memory)
data$Memory_Bandwidth <- gsub("\\GB/sec", "", data$Memory_Bandwidth)
data$Memory_Speed <- gsub("\\MHz", "", data$Memory_Speed)
data$Max_Power <- gsub("\\Watts", "", data$Max_Power)

print("# Chuyển các cột thành dạng numeric")
data$Release_Price <- as.numeric(data$Release_Price)
data$Core_Speed <- as.numeric(data$Core_Speed)
data$Memory <- as.numeric(data$Memory)
data$Memory_Bandwidth <- as.numeric(data$Memory_Bandwidth)
data$Memory_Speed <- as.numeric(data$Memory_Speed)
data$Max_Power <- as.numeric(data$Max_Power)

# Nhập dữ liệu cột Best_Resolution để tính number_of_pixels
GPU_data$Best_Resolution <- as.character(GPU_data$Best_Resolution)

# Tách cột độ phân giải thành hai phần: width và height
GPU_data$res_split <- strsplit(GPU_data$Best_Resolution, " x ")
GPU_data$width <- sapply(GPU_data$res_split, function(x) as.numeric(x[1]))
GPU_data$height <- sapply(GPU_data$res_split, function(x) as.numeric(x[2]))
# Tính tổng số pixel
data$number_of_pixels <- GPU_data$width * GPU_data$height

# Chuyển Manufacturer thành dạng factor
data$Manufacturer <- as.factor(data$Manufacturer)

print("# Kiểm tra số lượng và tỷ lệ dữ liệu khuyết")
library(inspectdf)
print(inspect_na(data))
str(data)

print("# Loại bỏ các hàng có giá trị thiếu")
data <- na.omit(data)
str(data)

# Xây dựng mô hình hồi quy tuyến tính
model <- lm(Release_Price ~ number_of_pixels + Core_Speed + Memory + 
            Memory_Bandwidth + Memory_Speed + Manufacturer + Max_Power, 
            data = data)

print(1)

# Tóm tắt kết quả mô hình
summary(model)

# Phân tích phần dư
residuals <- residuals(model)

# Biểu đồ histogram của phần dư
hist(residuals, main = "Residuals Histogram", xlab = "Residuals")

# Kiểm tra phân phối chuẩn (Shapiro-Wilk test)
shapiro.test(residuals)

# Cài đặt gói car (nếu chưa cài)
if(!require(car)) install.packages("car")

# Kiểm tra VIF
library(car)
vif(model)

# Kiểm tra Durbin-Watson
if(!require(lmtest)) install.packages("lmtest")
library(lmtest)

dwtest(model)

# Dự đoán với dữ liệu mới
new_data <- data.frame(
  number_of_pixels = c(1000, 1200),
  core_speed_value = c(1.5, 1.7),
  memory_value = c(8, 16),
  memory_bandwidth_value = c(256, 320),
  memory_speed_value = c(14, 16),
  manufacturer = factor(c("AMD", "ATI", "Intel", "Nvidia"), levels = levels(data$manufacturer)),
  max_power_value = c(120, 150)
)

predictions <- predict(model, newdata = new_data)
print(predictions)