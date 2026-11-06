# =====================================================================
#  후속 탐구: 숫자로 다시 보는 원조 경제 (1946~1990)
#  - 미국의 대한(對韓) 원조가 시작될 때부터 끝날 때까지의 흐름
#  - 원조와 경제성장률, 산업 구조, 수출의 변화 비교
#  실행 방법: 이 파일이 있는 폴더를 작업 폴더로 정한 뒤
#             RStudio에서 [Source] 버튼을 누르거나 source("analysis.R")
#  필요한 패키지: 없음 (기본 R만 사용)
# =====================================================================

# ---- 0. 한글 글꼴 설정 (운영체제에 따라 자동 선택) ----
if (.Platform$OS.type == "windows") {
  windowsFonts(KR = windowsFont("Malgun Gothic"))
  KR <- "KR"
} else if (Sys.info()[["sysname"]] == "Darwin") {
  KR <- "AppleGothic"
} else {
  KR <- "NanumGothic"
}
if (.Platform$OS.type != "windows") invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))
dir.create("charts", showWarnings = FALSE)

# ---- 1. 자료 불러오기 ----
d <- read.csv("data/aid_and_economy_1946_1990.csv", fileEncoding = "UTF-8-BOM")
# 단위: 원조액은 백만 달러(당시 가격), 성장률·비중은 %

save_png <- function(file, expr, w = 1600, h = 900) {
  png(file.path("charts", file), width = w, height = h, res = 150, family = KR)
  par(family = KR, mar = c(5, 5, 4, 5))
  expr
  invisible(dev.off())
}

# ---- 2. 그래프 1: 경제원조와 군사원조 (1946~1990) ----
save_png("1_원조_전체흐름.png", {
  m <- rbind(d$econ_aid, d$mil_aid)
  top <- max(colSums(m)) * 1.25
  b <- barplot(m, names.arg = d$year, col = c("grey20", "grey75"), border = NA, ylim = c(0, top),
               las = 2, cex.names = 0.7, ylab = "원조액 (백만 달러, 당시 가격)",
               main = "미국의 대한 원조 (1946~1990, 약정액 기준)")
  legend("topright", c("경제원조", "군사원조"), fill = c("grey20", "grey75"), border = NA, bty = "n")
  ev <- c(`1950` = "6·25 전쟁", `1956` = "경제원조 최고", `1961` = "USAID 설립", `1981` = "경제원조 종료")
  hy <- c(0.97, 0.90, 0.97, 0.90)   # 글자가 겹치지 않도록 높이를 번갈아 둠
  for (i in seq_along(ev)) {
    x <- b[d$year == as.numeric(names(ev)[i])]
    abline(v = x, lty = 3)
    text(x, top * hy[i], ev[[i]], pos = 4, cex = 0.8)
  }
})

# ---- 3. 그래프 2: 경제원조의 무상과 유상(차관) ----
e <- d[d$year <= 1981, ]
save_png("2_무상에서_유상으로.png", {
  m <- rbind(e$econ_grant, e$econ_loan)
  barplot(m, names.arg = e$year, col = c("grey25", "white"), las = 2, cex.names = 0.7,
          ylab = "경제원조 (백만 달러)", main = "경제원조: 갚지 않아도 되는 원조에서 갚아야 하는 차관으로")
  legend("topright", c("무상 원조", "유상 원조(차관)"), fill = c("grey25", "white"), bty = "n")
})

# ---- 4. 그래프 3: 경제원조의 종류 ----
save_png("3_경제원조_종류.png", {
  m <- rbind(e$relief, e$usaid_grant, e$usaid_loan, e$pl480)
  m[m < 0] <- 0
  barplot(m, names.arg = e$year, col = c("grey85", "grey45", "grey10", "white"), las = 2, cex.names = 0.7,
          ylab = "경제원조 (백만 달러)", main = "경제원조의 종류")
  legend("topright", c("긴급 구호 (점령지 구호·한국민간구호 등)", "USAID 무상 원조", "USAID 차관",
                       "잉여농산물 (PL480)"), fill = c("grey85", "grey45", "grey10", "white"), bty = "n", cex = 0.85)
})

# ---- 5. 그래프 4: 경제원조와 경제성장률 ----
g <- d[d$year >= 1953 & d$year <= 1981, ]
save_png("4_원조와_경제성장률.png", {
  b <- barplot(g$econ_aid, names.arg = g$year, col = "grey80", border = NA, las = 2, cex.names = 0.7,
               ylab = "경제원조 (백만 달러)", main = "경제원조와 경제성장률 (1953~1981)")
  par(new = TRUE)
  plot(b, g$gdp_growth_maddison, type = "o", pch = 16, axes = FALSE, xlab = "", ylab = "",
       xlim = range(b) + c(-0.6, 0.6), ylim = c(-5, 30))
  axis(4); mtext("실질 경제성장률 (%)", side = 4, line = 3)
  legend("topright", c("경제원조(막대)", "경제성장률(선)"), fill = c("grey80", NA), border = c(NA, NA),
         lty = c(NA, 1), pch = c(NA, 16), bty = "n")
})

# ---- 6. 그래프 5: 산업 구조와 수출 (1960~1990) ----
s <- d[d$year >= 1960, ]
save_png("5_산업구조와_수출.png", {
  plot(s$year, s$agri_share, type = "o", pch = 16, ylim = c(0, 52), xlab = "연도",
       ylab = "국내총생산(GDP)에서 차지하는 비중 (%)", main = "농업은 줄고 제조업과 수출은 늘다 (1960~1990)")
  lines(s$year, s$manuf_share, type = "o", pch = 17)
  lines(s$year, s$export_share, type = "o", pch = 1, lty = 2)
  abline(v = 1962, lty = 3); text(1962, 25, "1차 경제개발\n5개년 계획 시작", pos = 4, cex = 0.8)
  legend("topright", c("농림어업", "제조업", "수출"), pch = c(16, 17, 1), lty = c(1, 1, 2), bty = "n", horiz = TRUE)
})

# ---- 7. 숫자로 정리하기: 시기별 평균과 상관계수 ----
period <- cut(d$year, breaks = c(1945, 1953, 1957, 1961, 1971, 1981, 1990),
              labels = c("1946-53 해방·전쟁기", "1954-57 원조 최고기", "1958-61 원조 감소기",
                         "1962-71 경제개발 계획기", "1972-81 차관 중심기", "1982-90 원조 이후"))
tab <- aggregate(cbind(econ_aid, mil_aid, econ_loan, gdp_growth_maddison) ~ period, data = d,
                 FUN = function(x) round(mean(x, na.rm = TRUE), 1), na.action = na.pass)
names(tab) <- c("시기", "경제원조_평균", "군사원조_평균", "차관_평균", "성장률_평균")

r1 <- with(d[d$year >= 1954 & d$year <= 1961, ], cor(econ_aid, gdp_growth_maddison))
r2 <- with(d[d$year >= 1954 & d$year <= 1981, ], cor(econ_aid, gdp_growth_maddison))

write.csv(tab, "결과_시기별평균.csv", row.names = FALSE, fileEncoding = "UTF-8")
sink("결과_요약.txt")
cat("시기별 평균 (원조: 백만 달러, 성장률: %)\n"); print(tab, row.names = FALSE)
cat("\n경제원조액과 경제성장률의 상관계수\n")
cat("  1954~1961년:", round(r1, 2), "\n")
cat("  1954~1981년:", round(r2, 2), "\n")
cat("\n※ 상관계수는 -1~1 사이 값. 0에 가까우면 두 자료가 함께 움직인다고 보기 어렵다.\n")
cat("※ 상관관계가 있다고 해서 원인과 결과의 관계라고 할 수는 없다.\n")
sink()
cat(readLines("결과_요약.txt"), sep = "\n")
