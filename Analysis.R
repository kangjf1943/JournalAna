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
set.sample <- TRUE

# 载入停止词并且添加自定义停止词
data("stop_words")
stop_words <- stop_words %>% 
  rbind(tibble(word = c("research", "result", "results", "study"), 
               lexicon = "user"))

# 自定义函数 ----
# 函数：获取某个xml文件中的目标属性数据
# 输入：文件名
# 输出：列表
GetXml <- function(x) {
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
GetXmls <- function(xpath) {
  # 构建列表用于暂存各个xml文件
  xml_ls <- vector("list", 4)
  names(xml_ls) <- c("doi", "abstract", "fulltext", "ref")
  
  # 提取所有xml文件名并忽略meta数据
  xml_names <- dir(xpath)
  xml_names <- xml_names[!grepl("meta", xml_names)]
  setwd(xpath)
  for (x in xml_names) {
    single_xml <- GetXml(x)
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
# 参数：
# x：带有分析文本编号和内容的数据框
# col_abstract：文本内容列名
# col_doc：文本编号列名
# 输出：各篇文章词语计数规范数据，每一列为某个文本的某个词语的出现频数
CountWord <- function(x, col_abstract, col_doc) {
  # 摘要列分词
  x[[col_abstract]] <- as.character(x[[col_abstract]])
  tidy_abstract <- x %>% 
    unnest_tokens(word, abstract) %>%
    count(doi, word, sort = TRUE) 
  
  # 删除停止词
  tidy_abstract <- tidy_abstract %>% 
    anti_join(stop_words, by = "word")
  
  # 删除包含数字而不含字母的词语
  # 漏洞：原则上应该要删除的是纯数字或者数字与逗号和句号的组合，目前对正则表达式不甚了解，先如此处理。等到了解之后，还应该尝试删除意味不明的纯字母组合。
  tidy_abstract$keep <- !grepl(pattern = '[0-9]', x = tidy_abstract$word) & 
    grepl(pattern = '[a-z]', x = tidy_abstract$word)
  
  tidy_abstract <- tidy_abstract %>% 
    subset(keep == TRUE) %>% 
    # 将词语的各种变形都转化成词干形式并且合并相同的词
    # 漏洞：有些词干难以理解
    mutate(word = tm::stemDocument(word)) %>% 
    group_by(doi, word) %>% 
    summarise(n = sum(n)) %>% 
    ungroup() %>% 
    select(doi, word, n)
  
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
# x：数据框，第一列为文档编号，后续列为文档-词频矩阵
GetCosine <- function(x) {
  # 提取所需信息
  df <- x[2: ncol(x)]
  names.doc <- x[[1]]
  
  # Vector lengths
  vl <- sqrt(rowSums(df*df))
  
  # Matrix of all combinations
  comb <- t(combn(1:nrow(df), 2))
  
  # Compute cosine similarity for all combinations
  csim <- 
    apply(comb, 1, FUN = function(i) sum(apply(df[i, ], 2, prod))/prod(vl[i]))
  
  # Create a data.frame of the results
  res <- data.frame(
    doc_a = names.doc[comb[,1]],
    doc_b = names.doc[comb[,2]],
    csim = csim
  ) %>% 
    tibble()  # 转化为tibble格式
  
  return(res)
}

# 数据读取 ----
#. 各文所属section ----
# article、section和SI的对应关系
match.info <- read.csv("RawData/doi_si_section.csv") %>% 
  rename_with(tolower) %>% 
  as_tibble()

#. 原文信息 ----
# 读取文件夹中所有xml文件
xmlfiles <- dir("RawData/XmlData")
# 是否抽样：因为数据量大，对全体数据进行分析耗时太长。如果抽样的话，则选取部分数据进行试分析，在这个快速分析之后，再对全体数据进行分析，可以提高效率。
if(set.sample == TRUE) {
  set.seed(1234)
  xmlfiles <- sample(
    xmlfiles, 
    # 选择其中1/5的数据进行样本分析
    size = 0.2 * length(xmlfiles), 
    prob = rep_len(c(0.8, 0.4, 0.2, 0.1, 0.05), length.out = length(xmlfiles))
  )
}

# 构建列表存放各卷数据
text_ls <- vector("list", length(xmlfiles))
names(text_ls) <- xmlfiles

# 遍历读取xml文件夹中的所有文件
for (i in xmlfiles) {
  text_ls[[i]] <- GetXmls(paste0("RawData/XmlData/", i))
}

# 将各卷数据合并到一个数据框中并且加入section信息：此处，“section”就相当于一个生物群落，“doi”相当于一个样方，“abstract”将被分解成一个个“有机体”
art.textdata <- Reduce(rbind, text_ls) %>% 
  left_join(match.info, by = "doi") %>% 
  # 删除无section信息的条目
  # 漏洞：本分析主要目的之一是对无section的条目进行分类，所以后面可能还要把它们找回来
  subset(!is.na(section)) %>% 
  # 删除无abstract的文章
  subset(!is.na(abstract)) %>% 
  # 选择需要的列
  select(section, doi, abstract)

# 计算各个section对应的文章数量并且删除文章数少于50的section对应条目
tar.section <- art.textdata %>% 
  group_by(section) %>% 
  summarise(art_n = n()) %>% 
  ungroup() %>% 
  subset(art_n > 50) %>% 
  .$section
art.textdata <- art.textdata %>% 
  subset(section %in% tar.section)

# 生成包含词频的TF-ITF矩阵
art.tfidf <- 
  CountWord(art.textdata, col_abstract = "abstract", col_doc = "doi") %>% 
  # 生成tf、idf、tf-idf值列
  bind_tf_idf(word, doi, n) %>% 
  arrange(desc(tf_idf)) %>% 
  # 加入section信息
  left_join(match.info, by = "doi") %>% 
  select(section, doi, word, n, tf, idf, tf_idf) %>% 
  # 为了避免后面转换成宽数据时和文章词语重复，更改列名
  rename(art_section = section, art_doi = doi)

# 构建群落数据
art.comm <- art.tfidf %>% 
  select(art_section, art_doi, word, n) %>% 
  pivot_wider(id_cols = c(art_section, art_doi), names_from = word, 
              values_from = n, values_fill = 0)

# 计算多样性指数
art.div <- art.comm %>% 
  mutate(richness = apply(.[3:ncol(.)] > 0, 1, sum), 
         shannon = diversity(.[3:ncol(.)], index = "shannon")) %>% 
  select(art_section, art_doi, richness, shannon)

# 分析 ----
#. 描述性分析 ----
# 各卷论文数量
# 漏洞：未涵盖因无法解析被排除的xml文件，其实也就10-4和10-6文件夹中各1个文件
sapply(text_ls, nrow) %>% 
  plot(type = "l")

#. 多样性分析 ----
# 对比各section的多样性指数
ggplot(art.div) + 
  geom_boxplot(aes(art_section, richness)) + 
  theme(axis.text.x = element_text(angle = 90))
ggplot(art.div) + 
  geom_boxplot(aes(art_section, shannon)) + 
  theme(axis.text.x = element_text(angle = 90))

#. 主题模型 ----
# 生成term-document matrix
art.dtm <- cast_dtm(art.tfidf, art_doi, word, n)

# 主题分析
# 漏洞：暂时分成6个主题，其中包括一个NA
art.lda <- LDA(art.dtm, k = 6, control = list(seed = 1234))

# 转化成可阅读的主题数据框并取前十位可视化
tidy(art.lda, matrix = "beta") %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term)) + 
  geom_col() + 
  facet_wrap(~ topic, scales = "free")
# 漏洞：主题之间区分度不明显，且意味不明

# 各篇文章属于各个主题的概率
art.topic <- tidy(art.lda, matrix = "gamma") %>% 
  rename(art_doi = document) %>% 
  left_join(select(match.info, section, doi), by = c("art_doi" = "doi"))

# Section和LDA主题对应桑基图
# 用桑基图看各section被分到各LDA主题的概率之和及概率所占比例
art.topic.sankey.gamma <-  
  art.topic %>% 
  group_by(section, topic) %>% 
  summarise(gamma = sum(gamma)) %>% 
  mutate(gamma_prop = gamma / sum(gamma)) %>% 
  ungroup() %>% 
  mutate(node_1 = case_when(
    section == "Economic and Business Aspects of Sustainability" ~ 0,
    section == "Energy Sustainability" ~ 1,
    section == "Environmental Sustainability and Applications" ~ 2,
    section == "Geography and Sustainability" ~ 3,
    section == "Sustainable Agriculture" ~ 4,
    section == "Sustainable Education and Approaches" ~ 5,
    section == "Sustainable Engineering and Science" ~ 6,
    section == "Sustainable Management" ~ 7,
    section == "Sustainable Transportation" ~ 8,
    section == "Sustainable Urban and Rural Development" ~ 9,
    section == "Tourism, Culture, and Heritage" ~ 10, 
  ), node_2 = case_when(
    topic == "1" ~ 11, 
    topic == "2" ~ 12, 
    topic == "3" ~ 13, 
    topic == "4" ~ 14, 
    topic == "5" ~ 15, 
    topic == "6" ~ 16 
  ))
# 分别对概率和概率占比作图
art.topic.sankey.gamma %>% 
  plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(label = c(unique(.$section), 1:6)),
    link = list(source = .$node_1, target = .$node_2, value =  .$gamma)
  )
art.topic.sankey.gamma %>% 
  plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(label = c(unique(.$section), 1:6)),
    link = list(source = .$node_1, target = .$node_2, value =  .$gamma_prop)
  )

# 用桑基图看各section被分到各LDA主题的文章数量
art.topic.sankey.num <- art.topic %>% 
  # 选出每个文章所属概率最高的主题
  group_by(art_doi) %>% 
  top_n(1, gamma) %>%
  ungroup() %>% 
  # 如果对应最高概率大于80%则判断所属
  mutate(topic = case_when(
    gamma < 0.55 ~ "unclear", 
    TRUE ~ as.character(topic)
  )) %>%
  # 加入section信息并且根据section分组统计各section被归入各LDA主题的文章数
  group_by(section, topic) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n)) %>% 
  ungroup() %>% 
  # 生成桑基图数据
  mutate(node_1 = case_when(
    section == "Economic and Business Aspects of Sustainability" ~ 0,
    section == "Energy Sustainability" ~ 1,
    section == "Environmental Sustainability and Applications" ~ 2,
    section == "Geography and Sustainability" ~ 3,
    section == "Sustainable Agriculture" ~ 4,
    section == "Sustainable Education and Approaches" ~ 5,
    section == "Sustainable Engineering and Science" ~ 6,
    section == "Sustainable Management" ~ 7,
    section == "Sustainable Transportation" ~ 8,
    section == "Sustainable Urban and Rural Development" ~ 9,
    section == "Tourism, Culture, and Heritage" ~ 10, 
  ), node_2 = case_when(
    topic == "1" ~ 11, 
    topic == "2" ~ 12, 
    topic == "3" ~ 13, 
    topic == "4" ~ 14, 
    topic == "5" ~ 15, 
    topic == "6" ~ 16, 
    topic == "unclear" ~ 17
  ))

art.topic.sankey.num  %>% 
  plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(label = c(unique(.$section), 1:6, "unclear")),
    link = list(source = .$node_1, target = .$node_2, value =  .$n)
  )
# 按照比例数据作桑基图
art.topic.sankey.num %>% 
  plot_ly(
    type = "sankey",
    orientation = "h",
    node = list(label = c(unique(.$section), 1:6, "unclear")),
    link = list(source = .$node_1, target = .$node_2, value =  .$freq)
  )

#. 机器学习分类和section性质关系 ----
# 各section机器学习分类结果正确率
ml.cor.rate <- ab_svm_res %>% 
  group_by(section) %>% 
  summarise(cor_rate = sum(correct) / n()) %>% 
  ungroup()
# 漏洞：ab_svm_res来自机器学习分类脚本

# 和其他主题的相似性越低，分类越准确？
artsec.cosine <- art.topic.sankey.gamma %>% 
  pivot_wider(id_cols = "section", names_from = topic, 
              values_from = gamma_prop, values_fill = 0) %>%  
  GetCosine() %>% 
  group_by(doc_a) %>% 
  summarise(csim = mean(csim)) %>% 
  ungroup() %>% 
  rename(section = doc_a)
ml.cor.rate %>% 
  left_join(artsec.cosine, by = "section") %>% 
  ggplot() + geom_point(aes(csim, cor_rate))

