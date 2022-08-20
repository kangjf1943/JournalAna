# 概述 ----
# 分别以单卷、单篇文章、单个特刊为基本单元进行分析，主要分析内容包括：词频分析，主题模型，多样性指数等

# 包 ----
library(xml2)
library(tidyr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(patchwork)
library(forcats)
library(topicmodels)
library(vegan)

# 设置 ----
# 原始路径
set_oridir <- getwd()

# 载入停止词并且添加自定义停止词
data("stop_words")
stop_words <- stop_words %>% 
  rbind(tibble(word = c("research", "result", "results", "study"), 
               lexicon = "user"))

# 自定义函数 ----
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

# 数据读取 ----
# 读取文件夹中所有xml文件并做分析
xmlfiles <- dir("RawData/XmlData")

# 构建列表存放各卷数据
text_ls <- vector("list", length(xmlfiles))
names(text_ls) <- xmlfiles

# 遍历读取xml文件夹中的所有文件
for (i in xmlfiles) {
  text_ls[[i]] <- fun_getxmls(paste0("RawData/XmlData/", i))
}

# 分析 ----
#. 描述性分析 ----
# 各卷论文数量
# 漏洞：未涵盖因无法解析被排除的xml文件，其实也就10-4和10-6文件夹中各1个文件
sapply(text_ls, nrow) %>% 
  plot(type = "l")

#. 以文章为单元分析 ----
#.. 词频 ----
# 基于摘要内容分析各卷tf-idf变化
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

#.. 主题模型 ----
# 生成term-document matrix
dtm.art <- cast_dtm(tfidf, doi, word, n)

# 主题分析
# 漏洞：暂时分成6个主题，其中包括一个NA
lda.art <- LDA(dtm.art, k = 6, control = list(seed = 1234))

# 转化成可阅读的主题数据框并取前十位可视化
tidy(lda.art, matrix = "beta") %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term)) + 
  geom_col() + 
  facet_wrap(~ topic, scales = "free")
# 漏洞：主题之间区分度不明显，且意味不明

# 各篇文章属于各个主题的概率
topic.art <- tidy(lda.art, matrix = "gamma") %>% 
  rename(doi = document)

# 计算各卷在各个主题上的得分
# 加入各文章所属卷信息
for (i in 1:length(text_ls)) {
  text_ls[[i]]$vol <- names(text_ls[i])
}
text_df <- Reduce(rbind, text_ls)
topic_10_gamma <- topic_10_gamma %>% 
  left_join(select(text_df, doi, vol), by = "doi")

#.. 各文多样性指数 ----
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

# 计算各section和其他section之间的相似性平均值、中值和标准差
sec.art.cosine %>% 
  group_by(doc_a) %>% 
  summarise(mean = mean(csim), median = median(csim), sd = sd(csim)) %>% 
  ungroup() %>% 
  # 可视化各统计指标
  pivot_longer(cols = c("mean", "median", "sd"), names_to = "index") %>% 
  rename(section_id = doc_a) %>% 
  ggplot() + 
  geom_col(aes(section_id, value)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(.~ index, scale = "free_y", ncol = 1)

