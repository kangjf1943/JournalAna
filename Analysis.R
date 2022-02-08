library(xml2)
library(tidyr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(patchwork)
library(forcats)
library(topicmodels)

# Setting ----
set_oridir <- getwd()
# 载入停止词并且添加自定义停止词
data("stop_words")
stop_words <- stop_words %>% 
  rbind(tibble(word = c("research", "result"), 
               lexicon = "user"))

# Data import ----
# 函数：获取某个xml文件中的目标属性数据
# 输入：文件名
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
fun_getxmls <- function(xpath) {
  # 构建列表用于暂存各个xml文件
  xml_ls <- vector("list", 4)
  names(xml_ls) <- c("doi", "abstract", "fulltext", "ref")
  
  # 提取各个xml文件的信息并将其储存到xml_ls列表中
  xml_names <- dir(xpath)
  setwd(xpath)
  for (x in xml_names) {
    single_xml <- fun_getxml(x)
    for (j in names(xml_ls)) {
      xml_ls[[j]] <- c(xml_ls[[j]], single_xml[[j]])
    }
  }
  setwd(set_oridir)
  
  # 构建数据框用于存储结果并输出
  output <- data.frame(
    doi = xml_ls[["doi"]], 
    abstract = xml_ls[["abstract"]], 
    fulltext= xml_ls[["fulltext"]], 
    ref = xml_ls[["ref"]]
  )
  return(output)
}

# 测试：读取10年所有信息并做分析
xmlfiles <- dir("RawData/XmlData")

# 构建列表存放10年各卷数据
xmls_10 <- vector("list", length(xmlfiles))
names(xmls_10) <- xmlfiles

# 遍历读取xml文件夹中的所有文件
for (i in xmlfiles) {
  xmls_10[[i]] <- fun_getxmls(paste0("RawData/XmlData/", i))
}

# Analysis ----
## General description ----
# 各卷论文数量
# 待办：未涵盖因无法解析被排除的xml文件数量
sapply(xmls_10, nrow) %>% 
  plot(type = "l")

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
tfidf_10 <- vector("list", length(xmlfiles))
names(tfidf_10) <- xmlfiles

tidy_10 <- vector("list", length(xmlfiles))
names(tidy_10) <- xmlfiles

for (i in xmlfiles) {
  tidy_10[[i]] <- fun_wordcount(xmls_10[[i]], "abstract", i)
}

tfidf_10 <- Reduce(rbind, tidy_10) %>% 
  bind_tf_idf(word, doc, n) %>% 
  arrange(desc(tf_idf))

# 输出绝对词频前10位的词及对应的图
tfidf_10 %>%
  group_by(doc) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(n, fct_reorder(word, n))) +
  facet_wrap(~ doc, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
# 待办：需要排除各卷共有的词，以显示区别，以及消除用词频率和论文数量的影响

# 输出tf-idf前10位的词及对应的图
tfidf_10 %>%
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
tidy_10_df <- Reduce(rbind, tidy_10)

# 生成term-document matrix
dtm_10_df <- cast_dtm(tidy_10_df, doc, word, n)

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

## Article as basic unit ----
# 待办：以10年第一卷为例分析
# 构建一词一行规范数据
tidy_10_1 <- xmls_10$`10-1` %>% 
  select(doi, abstract) %>% 
  mutate(abstract = as.character(abstract)) %>%
  unnest_tokens(word, abstract) %>% 
  anti_join(stop_words) %>% 
  count(doi, word, sort = TRUE)

# 直接做主题模型
# 生成DTM矩阵
dtm_10_1 <- tidy_10_1 %>% 
  cast_dtm(doi, word, n)

# 通过LDA模型划分主题
lda_10_1 <- LDA(dtm_10_1, k = 5, control = list(seed = 1234))

# 读取LDA模型结果
topic_10_1_beta <- tidy(lda_10_1, matrix = "beta")
topic_10_1_gamma <- tidy(lda_10_1, matrix = "gamma")

# 可视化主题结果：各个主题主要有哪些关键词
topic_10_1_beta %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 15) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term, beta, topic, sep = "-")) %>% 
  ggplot() + 
  geom_col(aes(beta, term)) + 
  facet_wrap(~topic, scales = "free")

# 计算本卷总体主题倾向：加总各个主题的概率值
topic_10_1_gamma %>% 
  group_by(topic) %>% 
  summarise(score = sum(gamma)) %>%
  ungroup()
