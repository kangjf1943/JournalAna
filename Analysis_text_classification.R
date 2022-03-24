library(RTextTools)

# Data import ----
# 读取董构建的doi-section-si数据
si_info <- read.csv("RawData/doi_si_section.csv") %>% 
  rename_with(tolower)

# Function ----

# 函数：将数据集分成训练集和测试集
# 概述：从各个section中，抽取20%作为测试集
# 输入：带有abstract和section列的数据框
# 输出：带有基于分组抽样选定训练集、测试集、待分类标签列的数据
fun_testsample <- function(x) {
  # 分为两部分：section已知和未知
  # section信息未知的部分
  x_unknown <- subset(x, is.na(section))
  x_unknown$set <- "unknown"
  
  # section信息已知的部分
  x_known <- subset(x, !is.na(section))
  
  # 对section信息已知部分的各section抽80%作为训练集
  x_known_train <- x_known %>% 
    group_by(section) %>% 
    slice_sample(prop = 0.8) %>% 
    ungroup() %>% 
    mutate(set = "train")
  
  # 其他的则作为测试集
  x_known_test <- x_known %>% 
    anti_join(x_known_train) %>% 
    mutate(set = "test")
  
  # 合并列表并输出
  x_output <- Reduce(rbind, list(x_known_train, x_known_test, x_unknown))
  return(x_output)
}

# 构建函数：基于支持向量机对文档进行分类
# 输入：文本及其分类数据框，带有各条数据性质的列
# 输出：测试或者待分类条目的分类结果
fun_textclass <- function(x, names_train, names_test) {
  # 根据选入的训练数据和测试数据选取输入数据框的子集
  x <- subset(x, set %in% c(names_train, names_test))
  
  # 建立DTM矩阵
  # 构建各文本文档对应的各词语数据框
  term_df <- unnest_tokens(x, word, abstract)
  # 对各词语计数
  word_count <- count(term_df, section, si, word, sort = TRUE) %>% 
    .[order(.$section, na.last = TRUE), ]
  # 建立DTM矩阵
  dtm <- cast_dtm(word_count, si, word, n)
  # 建立包含训练数据和待测试数据的container
  # 根据set列的数据性质区分训练数据和测试数据
  container <- create_container(
    dtm, x$section, 
    trainSize = as.numeric(rownames(x)[which(x$set %in% names_train)]),
    testSize = as.numeric(rownames(x)[which(x$set %in% names_test)]), 
    virgin = FALSE)
  # 生成支持向量机分类模型
  svm_model <- train_model(container, "SVM")
  # 生成分类结果
  svm_res <- classify_model(container, svm_model) %>% 
    cbind(x[x$set %in% names_test, ])
  
  return(svm_res)
}

# 以单个特刊为基本单位进行分析 ----
# 假设对已有section信息的特刊的归类是合理的，以这些特刊为训练数据集，对没有section信息的特刊进行分类
# 合并各卷论文数据并将各条目分成训练集和测试集
# 董构建的特刊数据集结果包含2456本特刊，而目前分析的各卷文章共包含于1107本特刊中
# 待办：text_ls是由Analysis.R构建的数据
text_df <- Reduce(rbind, text_ls) %>% 
  select(-fulltext, - ref) %>% 
  # 接入secssion和si数据
  left_join(si_info, by = "doi") %>% 
  # 合并同一特刊中的摘要
  group_by(section, si) %>% 
  summarise(abstract = paste0(abstract, collapse = "")) %>% 
  ungroup() %>% 
  # 去除非特刊投稿
  subset(!is.na(si)) %>% 
  #去除重复特刊名：有3份特刊被同时归入“无分类”及某些特刊中
  subset(!duplicated(.$si)) %>% 
  .[order(.$section, na.last = TRUE), ] %>% 
  # 将数据分成训练集、测试集、待分类数据
  fun_testsample()

# 评价模型质量 ----
svm_res <- fun_textclass(text_df, "train", "test")

# 看分类结果和实际结果是否一致：是否一致跟模型质量有关，也跟原本分类是否合理有关
svm_res$correct <- svm_res$SVM_LABEL == svm_res$section
table(svm_res$correct)
# 查看分类正确组合错误组之间模型确定率的关系
ggplot(svm_res) + 
  geom_boxplot(aes(correct, SVM_PROB)) 

# 函数：生成分类结果诊断图
# 输入：x_text_df，带各分析单元索虎section信息的数据框；x_svm_res，分类结果数据框
# 输出：分类结果诊断图，包括各section样本数量、各section分类确定率、各section实际分类准确率
fun_pltclass <- function(x_text_df, x_svm_res) {
  # 各样本量之间的关系
  plt_size_sec <- ggplot(x_text_df[!is.na(x_text_df$section), ]) + 
    geom_col(aes(section, 1)) + 
    theme(axis.text.x = element_blank())
  # 查看不同section的准确率差异和实际判断正误率的差异
  plt_prob_sec <- ggplot(x_svm_res) + 
    geom_boxplot(aes(section, SVM_PROB)) + 
    theme(axis.text.x = element_blank())
  plt_correct_sec <- ggplot(x_svm_res) + 
    geom_col(aes(section, factor(correct), fill = correct)) + 
    theme(axis.text.x = element_text(angle = 90))
  plt_size_sec / plt_prob_sec / plt_correct_sec
}
fun_pltclass(text_df, svm_res)

# 对未归类特刊进行分类
svm_res_unknown <- fun_textclass(text_df, "train", "unknown")
# 看看自动分类的数量分布
ggplot(svm_res_unknown) + 
  geom_col(aes(SVM_LABEL, 1)) + 
  theme(axis.text.x = element_text(angle = 90))


