library(xml2)
library(tidyr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(patchwork)
library(forcats)
library(topicmodels)
library(vegan)

# Setting ----
set_oridir <- getwd()
# 载入停止词并且添加自定义停止词
data("stop_words")
stop_words <- stop_words %>% 
  rbind(tibble(word = c("research", "result", "results", "study"), 
               lexicon = "user"))

# Data import ----
# 函数：获取某个xml文件中的目标属性数据
# 输入：文件名
# 输出：列表
fun_getxml <- function(x) {
  # 获取一篇文章中的全部原始数据
  # 待办：为了排除无法解析的xml，此处用try()函数
  rawdata <- try(read_xml(x))
  if (class(rawdata) != "try-error") {
    # 获取该文章中的各类目标信息
    # doi
    doi_path <- xml_find_all(rawdata, ".//article-id")
    doi <- xml_text(doi_path)[1]
    # 摘要
    abstract <- xml_text(xml_find_all(rawdata, ".//abstract"))
    # 全文
    fulltext <- xml_text(xml_find_all(rawdata, ".//body"))
    # remove ref in the text
    fulltext <- gsub("\\[.*\\]","",fulltext)
    # 参考文献
    ref <- xml_text(xml_find_all(rawdata, ".//ref-list"))
    
    # 将这些信息组合成列表
    output <- list(
      doi = doi, 
      abstract = abstract, 
      fulltext= fulltext, 
      ref = ref
    )
    
    # 替换缺失数据为NA
    for (i in names(output)) {
      if (length(output[[i]]) == 0) output[[i]] <- NA
    }
  } else {
    # 如果无法解析xml则直接生成全为NA的结果
    output <- list(
      doi = NA, 
      abstract = NA, 
      fulltext= NA, 
      ref = NA
    )
  }
  
  return(output)
}

# 函数：批量获取xml文件的目标属性数据
# 输入：目标文件夹
# 输出：包含tibble的列表
fun_getxmls <- function(xpath) {
  # 构建列表用于暂存各个xml文件
  xml_ls <- vector("list", 4)
  names(xml_ls) <- c("doi", "abstract", "fulltext", "ref")
  
  # 提取所有xml文件名并忽略meta数据
  xml_names <- dir(xpath)
  xml_names <- xml_names[!grepl("meta", xml_names)]
  setwd(xpath)
  for (x in xml_names) {
    single_xml <- fun_getxml(x)
    for (j in names(xml_ls)) {
      xml_ls[[j]] <- c(xml_ls[[j]], single_xml[[j]])
    }
  }
  setwd(set_oridir)
  
  # 构建数据框用于存储结果
  output <- data.frame(
    doi = xml_ls[["doi"]], 
    abstract = xml_ls[["abstract"]], 
    fulltext= xml_ls[["fulltext"]], 
    ref = xml_ls[["ref"]]
  )
  
  # 将结果转化为tibble格式并输出
  output <- as_tibble(output)
  return(output)
}

# 读取文件夹中所有xml文件并做分析
xmlfiles <- dir("RawData/XmlData")

# 构建列表存放10年各卷数据
text_ls <- vector("list", length(xmlfiles))
names(text_ls) <- xmlfiles

# 遍历读取xml文件夹中的所有文件
for (i in xmlfiles) {
  text_ls[[i]] <- fun_getxmls(paste0("RawData/XmlData/", i))
}

# General description ----
# 各卷论文数量
# 待办：未涵盖因无法解析被排除的xml文件，其实也就10-4和10-6文件夹中各1个文件
sapply(text_ls, nrow) %>% 
  plot(type = "l")

# Volume-based analysis ----
## Term frequency ----
# 基于摘要内容分析各卷tf-idf变化

# 函数：词频统计
# 输入：带有文本内容列的数据框
# 输出：各卷词语计数规范数据
fun_wordcount <- function(x, col_abstract, col_doc) {
  # 摘要列分词
  x[[col_abstract]] <- as.character(x[[col_abstract]])
  tidy_abstract <- x %>% 
    unnest_tokens(word, abstract) %>%
    count(word, sort = TRUE) %>% 
    mutate(doc = col_doc) %>% 
    select(doc, word, n)
  
  # 删除停止词
  tidy_abstract <- tidy_abstract %>% 
    anti_join(stop_words)
  
  return(tidy_abstract)
}

# 生成各卷对应的tf-idf数据框
tfidf <- vector("list", length(xmlfiles))
names(tfidf) <- xmlfiles

tidy_ls <- vector("list", length(xmlfiles))
names(tidy_ls) <- xmlfiles

for (i in xmlfiles) {
  tidy_ls[[i]] <- fun_wordcount(text_ls[[i]], "abstract", i)
}

tfidf <- Reduce(rbind, tidy_ls) %>% 
  bind_tf_idf(word, doc, n) %>% 
  arrange(desc(tf_idf))

# 输出绝对词频前10位的词及对应的图
tfidf %>%
  group_by(doc) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(n, fct_reorder(word, n))) +
  facet_wrap(~ doc, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
# 待办：需要排除各卷共有的词，以显示区别，以及消除用词频率和论文数量的影响

# 输出tf-idf前10位的词及对应的图
tfidf %>%
  group_by(doc) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(tf_idf, fct_reorder(word, tf_idf))) +
  facet_wrap(~ doc, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
# 待办：原理上讲，这几个图展示了各卷区别于其他卷的地方，但是其实可能高估了罕见词
# 的作用，比如说10-4中出现了“Quanzhou”，但是其实只是因为有两篇文章涉及到泉州而已
# 导致这种现象的原因可能是各卷的主题都太综合了，涉及常见环境主题词的摘要都太多
# 所以到底用绝对词频还是用tf-idf来表示每卷的侧重主题其实可能有争议，或者是自己
# 弄一个新的指标出来？或者是tf、idf和tf-idf几个都可视化一下？
# 或者说，最小分析单元不应该是卷而应该是文章？

## Topic model ----
# 将各卷词频统计混合起来
tidy_ls_df <- Reduce(rbind, tidy_ls)

# 生成term-document matrix
dtm_10_df <- cast_dtm(tidy_ls_df, doc, word, n)

# 主题分析
lda_10 <- LDA(dtm_10_df, k = length(xmlfiles), control = list(seed = 1234))
lda_10

# 转化成可阅读的主题数据框
topic_10_beta <- tidy(lda_10, matrix = "beta")

# 取前十位可视化
topic_10_beta %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term)) + 
  geom_col() + 
  facet_wrap(~ topic, scales = "free")
# 待办：结果区分度不明显

# 各卷属于各个主题的概率
topic_10_gamma <- tidy(lda_10, matrix = "gamma")

topic_10_gamma %>% 
  ggplot() + 
  geom_col(aes(topic, gamma)) + 
  facet_wrap(~document)

# Article-based analysis ----
## Term frequency ----
# 基于摘要内容分析各卷tf-idf变化

# 函数：词频统计
# 输入：带有文本内容列的数据框
# 输出：各篇文章词语计数规范数据
fun_wordcount <- function(x, col_abstract, col_doc) {
  # 摘要列分词
  x[[col_abstract]] <- as.character(x[[col_abstract]])
  tidy_abstract <- x %>% 
    unnest_tokens(word, abstract) %>%
    count(doi, word, sort = TRUE) %>% 
    mutate(doc = col_doc) %>% 
    select(doc, doi, word, n)
  
  # 删除停止词
  tidy_abstract <- tidy_abstract %>% 
    anti_join(stop_words)
  
  return(tidy_abstract)
}

# 生成各文章对应的tf-idf数据框
tfidf <- vector("list", length(xmlfiles))
names(tfidf) <- xmlfiles

tidy_ls <- vector("list", length(xmlfiles))
names(tidy_ls) <- xmlfiles

for (i in xmlfiles) {
  tidy_ls[[i]] <- fun_wordcount(text_ls[[i]], "abstract", i)
}

tfidf <- Reduce(rbind, tidy_ls) %>% 
  bind_tf_idf(word, doi, n) %>% 
  arrange(desc(tf_idf))

# 输出绝对词频前10位的词及对应的图
# tfidf %>%
#   group_by(doc) %>%
#   slice_max(n, n = 10) %>%
#   ungroup() %>%
#   ggplot() +
#   geom_col(aes(n, fct_reorder(word, n))) +
#   facet_wrap(~ doc, ncol = 2, scales = "free") +
#   labs(x = "tf-idf", y = NULL)
# 待办：如果输出每篇文章前10位的词语，结果就太长了

# 输出tf-idf前10位的词及对应的图
# tfidf %>%
#   group_by(doc) %>%
#   slice_max(tf_idf, n = 10) %>%
#   ungroup() %>%
#   ggplot() +
#   geom_col(aes(tf_idf, fct_reorder(word, tf_idf))) +
#   facet_wrap(~ doc, ncol = 2, scales = "free") +
#   labs(x = "tf-idf", y = NULL)
# 待办：如果输出每篇文章前10位的词语，结果就太长了

## Topic model ----
# 将各卷词频统计混合起来
tidy_ls_df <- Reduce(rbind, tidy_ls)

# 生成term-document matrix
dtm_10_df <- cast_dtm(tidy_ls_df, doi, word, n)

# 主题分析
# 待办：分成和section数量等同的28个主题，其中包括一个NA
lda_10 <- LDA(dtm_10_df, k = 28, control = list(seed = 1234))
lda_10

# 转化成可阅读的主题数据框
topic_10_beta <- tidy(lda_10, matrix = "beta")

# 取前十位可视化
topic_10_beta %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term)) + 
  geom_col() + 
  facet_wrap(~ topic, scales = "free")
# 待办：结果区分度不明显

# 各篇文章属于各个主题的概率
topic_10_gamma <- tidy(lda_10, matrix = "gamma") %>% 
  rename(doi = document)

# topic_10_gamma %>% 
#   ggplot() + 
#   geom_col(aes(topic, gamma)) + 
#   facet_wrap(~ doi)
# 待办：文章数量太多，无法把每个图都画出来

# 计算各卷在各个主题上的得分
# 加入各文章所属卷信息
for (i in 1:length(text_ls)) {
  text_ls[[i]]$vol <- names(text_ls[i])
}
text_df <- Reduce(rbind, text_ls)
topic_10_gamma <- topic_10_gamma %>% 
  left_join(select(text_df, doi, vol), by = "doi")

# 计算各卷各主题得分
topic_10_gammascore_vol <- topic_10_gamma %>% 
  group_by(topic, vol) %>% 
  summarise(score = sum(gamma)) %>% 
  ungroup()

# 可视化各卷各主题得分
ggplot(topic_10_gammascore_vol) + 
  geom_col(aes(topic, score)) + 
  facet_wrap(.~ vol)
# 待办：同一卷在各个主题上的得分相似？

## Diversity index of each article ----
# 基于文章为单元的词频矩阵
sec.tfidf <- 
  tfidf %>% 
  # 加入各文章对应section的数据
  left_join(si_info, by = "doi") %>% 
  group_by(section, word) %>% 
  summarise(n = sum(n)) %>% 
  arrange(section, -n) %>% 
  mutate(rank = 1:n()) %>% 
  ungroup() %>% 
  # 去除无section信息的条目
  subset(section != "NA")

# 作等级-丰度图：横轴延伸长度表示特有词语数量，斜度表示词语频率分布的均匀程度
ggplot(sec.tfidf) + 
  geom_line(aes(rank, n)) + 
  facet_wrap(.~ section, nrow = 3)

# 函数：基于词频数据计算各文本分组的多样性指数
# 输出：
# 参数：
# x：带有词语、对应频率、分组信息的数据，各列命名有要求
GetDiv <- function(x) {
  # 分组名称
  grp.names <- unique(x$section)
  # 空列表用于储存结果
  div.ls <- vector("list", length = length(grp.names))
  names(div.ls) <- grp.names
  
  # 对各文本分组计算多样性指数
  for (i in grp.names) {
    # 选择目标组子集
    x.sub <- subset(x, section == i) %>% 
      # 构建“群落数据”，列名为词语，数据为词频
      select(word, n) %>% 
      pivot_wider(names_from = word, values_from = n, values_fill = 0)
    # 计算多样性指数
    div.ls[[i]] <- tibble(
      abundance = sum(x.sub), 
      richness = ncol(x.sub),
      shannon = diversity(x.sub, index = "shannon"), 
      simpson = diversity(x.sub, index = "simpson"),
      evenness = shannon / log(richness)
    )
  }
  div.df <- tibble(
    section = names(div.ls), 
    Reduce(rbind, div.ls)
  )
  return(div.df)
}

# 计算各篇文章词语多样性指数并可视化
sec.tfidf.div <- GetDiv(sec.tfidf)
sec.tfidf.div %>% 
  # 转化成长数据
  pivot_longer(
    cols = c("abundance", "richness", "shannon", "simpson", "evenness"), 
    names_to = "index", values_to = "value"
  ) %>% 
  ggplot() + 
  geom_col(aes(section, value)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~ index, scales = "free_y", ncol = 1)

sec.art.dtm <- sec.tfidf %>% 
  rename(section_id = section) %>% 
  pivot_wider(
    id_cols = section_id, names_from = word, values_from = n, values_fill = 0
  )

# 函数：计算文本之间的cosine相似度
# 备注：改编自网上的代码（https://stackoverflow.com/questions/52720178/cosine-similarity-of-documents）
# 参数：
# df：文档-词频矩阵
GetCosine <- function(df) {
  df <- data.frame(df)
  # 将文档-词频矩阵的分组列转化为其行名
  rownames(df) <- df$section_id
  df$section_id <- NULL
  # Vector lengths
  vl <- sqrt(rowSums(df*df))
  
  # Matrix of all combinations
  comb <- t(combn(1:nrow(df), 2))
  
  # Compute cosine similarity for all combinations
  csim <- 
    apply(comb, 1, FUN = function(i) sum(apply(df[i, ], 2, prod))/prod(vl[i]))
  
  # Create a data.frame of the results
  res <- data.frame(
    doc_a = rownames(df)[comb[,1]],
    doc_b = rownames(df)[comb[,2]],
    csim = csim
  ) %>% 
    tibble()  # 转化为tibble格式
  
  return(res)
}

# 计算大概需要花两分钟
sec.art.cosine <- GetCosine(sec.art.dtm)
# 补全矩阵另一半
sec.art.cosine <- rbind(
  sec.art.cosine, 
  tibble(doc_a = sec.art.cosine$doc_b, 
         doc_b = sec.art.cosine$doc_a, 
         csim = sec.art.cosine$csim)
) 
# 补全各主题和自己的相似性的部分
sec.art.cosine <- rbind(
  sec.art.cosine, 
  tibble(doc_a = unique(sec.art.cosine$doc_a), 
         doc_b = unique(sec.art.cosine$doc_a), 
         csim = 1)
)

# 可视化
ggplot(sec.art.cosine) + geom_tile(aes(x = doc_a, y = doc_b, fill = csim)) + 
  scale_fill_gradientn(colors = terrain.colors(10)) + 
  theme(axis.text.x = element_text(angle = 90))

# 计算各section和其他section之间的相似性平均值和中值
sec.art.cosine %>% 
  group_by(doc_a) %>% 
  summarise(mean = mean(csim), median = median(csim)) %>% 
  ungroup() %>% 
  # 可视化平均值和中值
  pivot_longer(cols = c("mean", "median"), names_to = "index") %>% 
  rename(section_id = doc_a) %>% 
  ggplot() + 
  geom_col(aes(section_id, value)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~ index, ncol = 1)

# Special issue-based analysis ----
# 从结果来看各卷在各主题上的得分还比较均衡，那如果是按照特刊来做主题模型分类呢？
si_info <- read.csv("RawData/doi_si_section.csv") %>% 
  rename_with(tolower) %>% 
  as_tibble()

# 合并主题模型结果和各文章所属特刊信息
topic_10_gamma <- topic_10_gamma %>% 
  left_join(si_info, by = "doi")
# 待办：存在不属于任何特刊的文章可以理解，但是否有不属于任何section的文章？

# 计算各特刊各主题得分
topic_10_gammascore_si <- topic_10_gamma %>% 
  group_by(topic, si) %>% 
  summarise(score = sum(gamma)) %>% 
  ungroup()

# 可视化各卷各主题得分
# 待办：由于涉及特刊太多，无法把每个图都可视化出来

# 计算各section各主题得分
topic_10_gammascore_section <- topic_10_gamma %>% 
  group_by(topic, section) %>% 
  summarise(score = sum(gamma)) %>% 
  ungroup() %>% 
  group_by(section) %>% 
  mutate(score_std = score / max(score)) %>% 
  ungroup()

# 计算各section的标准差
topic_10_gammascore_section_sd <- topic_10_gammascore_section %>% 
  group_by(section) %>% 
  summarise(sd = sd(score_std)) %>% 
  ungroup() %>% 
  arrange(-sd)

# 将各section按照标准差从高到低排序
lvl_section <- topic_10_gammascore_section_sd$section
topic_10_gammascore_section <- topic_10_gammascore_section %>% 
  mutate(section = factor(section, levels = lvl_section))

# 可视化各卷各主题得分
ggplot(topic_10_gammascore_section) + 
  geom_col(aes(topic, score_std)) + 
  facet_wrap(.~ section)
# 通过颜色深浅可视化
ggplot(topic_10_gammascore_section) + 
  geom_tile(aes(topic, section, fill = score_std)) + 
  scale_fill_continuous(low = "white", high = "red")

# 是否有明显的聚集现象：同一个特刊中的文章是否基本被归在同一个主题下？
ggplot(topic_10_gamma) + 
  geom_boxplot(aes(as_factor(topic), gamma)) + 
  facet_wrap(.~ section)

# 看看各个doi或者是section内的文章是否表现出相似性
# 属于同一个section的特刊是否表现出相似性？
# 假说：
# 各个特刊内或者各个section内的文章表现出相似性
# 各个特刊内的文章的相似性应该高于各个section内的文章的相似性
# 相同section的特刊之间表现出相似格局
# 待办：参数检验的前提到底是什么？
