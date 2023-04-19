* 变量设计～～～～～
* 一、同群效应的衡量
* 1. 变量整理
gen year = substr(accper,1,4)
destring year,replace
drop accper //获得年份

label variable stkcd "股票代码"
label variable fixedasset "固定资产净额"
label variable construction "在建工程净额"
label variable intangible "无形资产净额"
label variable year "会计年度"

sort stkcd year //排序
save "/Users/soyo/Downloads/毕业论文数据库/固定+在建+无形.dta", replace

** 同理整理总资产
use "/Users/soyo/Downloads/毕业论文数据库/总资产.dta"
gen year = substr(enddate,1,4)
destring year,replace
drop enddate 

** 数据文件合并
sort stkcd year
merge m:m year stkcd using "/Users/soyo/Downloads/毕业论文数据库/固定+在建+无形.dta"
drop if _merge!=3 //删除缺失值
drop if stkcd == . //删除空白值
drop _merge

* 2. 变量计算
** 生成滞后一期变量
destring stkcd, replace
xtset stkcd year //定义面板数据
gen lstkcd = l.stkcd
drop lstkcd 
gen lfix = l.fixedasset
gen lcon = l.construction
gen lintan = l.intangible
gen lta = l.totalassets
label variable lfix "滞后一期固定资产"
label variable lcon "滞后一期在建工程"
label variable lintan "滞后一期无形资产"
label variable lta "滞后一期总资产"

** 计算企业投资水平
gen inv = ((fixedasset+construction+ intangible)-(lfix+ lcon+ lintan))/lta
label variable inv "企业投资水平"
drop if inv == .

** 计算行业内企业规模前10%的企业投资加权平均数
gsort year industrycode -totalassets
bysort year industrycode : gen ta123 = _n //对企业规模排序
bys year industrycode : egen sumcode = count(stkcd) //统计行业内企业数量
bysort year industrycode : gen top10 = 1 if ta123/sumcode <= 0.1 //只保留前10%
save "/Users/soyo/Downloads/毕业论文数据库/企业投资水平.dta", replace
drop if top10 == . //舍弃上市企业小于10的行业及公司另外保存文件
bysort year industrycode: egen weight = pc(totalassets),prop
bysort year industrycode: egen leadinv = sum(weight*inv)
sort stkcd year
merge m:m year industrycode  using "/Users/soyo/Downloads/毕业论文数据库/企业投资水平.dta"
sort stkcd year

** 计算羊群水平
drop if top10 == 1 //只研究投资水平为后90%的企业
bysort year stkcd : gen herdinv = abs(inv - leadinv) //绝对值

* 二、崩盘风险计算
* 1.变量整理
** 生成周度编码
gen weekcode = _n - 2
label variable weekcode "周份编码"
gen year = substr(trddt,1,4) //截取年份
destring stkcd, replace
tostring wknum,replace //周数转化为字符
gen str6 id_rvsd = wknum
replace id_rvsd = "0" + id if length(id) == 1 //补足个位数周
gen weekcode = year + "-" + id_rvsd //生成同样周度
destring weekcode, replace

** 生成滞后的市场回报率
tsset tr //排列时间序列数据
gen lmrkrtr = l.cwretwdos
gen lmrkrtr = l2.cwretwdos
gen l2mrkrtr = l2.cwretwdos
gen fmrkrtr = f.cwretwdos
gen f2mrkrtr = f2.cwretwdos
drop if lmrkrtr == .
drop if l2mrkrtr == .
drop if f2mrkrtr == .
drop if fmrkrtr == .
label variable lmrkrtr "滞后一期市场回报"
label variable l2mrkrtr "滞后二期市场回报"
label variable fmrkrtr "提前一期市场回报"
label variable f2mrkrtr "提前二期市场回报"
save "/Users/soyo/Downloads/毕业论文数据库/周市场回报率.dta", replace

** 文件合并整理
merge m:m weekcode using "/Users/soyo/Downloads/毕业论文数据库/周市场回报率.dta"
sort _merge 
drop if _merge == 1
drop _merge 
drop trddt wkn id_
destring year, replace
sort weekcode 
label variable stkcd "股票编码"
label variable wretwd "周个股回报率"
label variable year "年度"
label variable weekcode "交易周"

* 2.崩盘风险的衡量
** 计算周特质收益率
reg wretwd lmrkrtr l2mrkrtr cwretwdos fmrkrtr f2mrkrtr
predict e,r //得到残差
drop if e == .
label variable e "残差"
g W = ln(1+e)
label variable W "周特质收益率"

** 计算NCSKEW
by stkcd year : g indtrawek = _n //该周是该股票在该年度交易的第几周
by stkcd year : egen intweek = max(indtrawek)
drop indtrawek
lab variable intweek "股票在该年度的交易周数"
drop if intweek <= 30 //删除交易周数小于30的样本
by stkcd year : egen NCSKEW1 = sum(W^3)
by stkcd year : egen NCSKEW2 = sum(W^2)
by stkcd year : g NCSKEW3 = - (intweek*((intweek-1)^(3/2))*NCSKEW1)
by stkcd year : g NCSKEW4 = (intweek-1)*(intweek-2)*(NCSKEW2^(3/2))
by stkcd year : g NCSKEW = NCSKEW3/NCSKEW4 
sum NCSKEW //计算复杂，检查平均数是否离谱
label variable NCSKEW "收益负偏态系数"

** 计算周回报率大小于平均的周数
by stkcd year : g nd = 1 if W < AVW
by stkcd year : g nu = 1 if W > AVW
by stkcd year : egen ndd = sum(nd)
by stkcd year : egen nuu = sum(nu)
label variable nd "周回报率低于当年周回报率均值"
label variable ndd "周回报率低于当年周回报率均值的周数"
label variable nu "周回报率高于当年周回报率均值"
label variable nuu "周回报率高于当年周回报率均值的周数"

** 计算DUVOL
gen dww = W if nd == 1
replace dww =0 if dww == .
gen uww = W if nu == 1
replace uww =0 if uww == .
by stkcd year : egen DWD = sum (dww^2)
by stkcd year : egen UWU = sum (uww^2)
by stkcd year : gen DUVOL = ln(((nuu-1)*DWD)/((ndd-1)*UWU))
label variable DUVOL "收益上下波动率"

* 三、高管特征的衡量
* 1.变量整理
** 年度编码
destring st, replace
gen year = substr(re,1,4)
destring year,replace
drop rep
drop if year == .
label variable year "年份"

** 学历编码
replace degree = 4 if degree == 7 //EMBA与MBA并入硕士
tab degree
drop if degree == 6 //排除以其他形式公布的学历，如荣誉博士、函授等
label variable degree "1，中专及以下；2，大专；3，本科；4，硕士研究生（包括MBA/EMBA）；5，博士及以上。"

** 性别编码
replace gender = "0" if gender == "男"
replace gender = "1" if gender == "女"
destring gender, replace

* 2.均值计算
sort stkcd year
bys stkcd year : egen boardnum = count(personid)
label variable boardnum "高管团队人数"

bys stkcd year : egen bdegree = mean(degree)
label variable bdegree "高管平均学历"
bys stkcd year : egen bage = mean(age)
label variable bage "高管平均年龄"
bys stkcd year : egen b1gender = sum(gender) //辅助计算
bys stkcd year : gen bgender = b1gender/boardnum
drop b1gender
label variable bgender "高管女性占比"
sum boardnum bdegree bage bgender

* 3.删除多余数据
drop gender age degree
duplicates drop
tsset s y
g lbdgree = l.bd
label variable lbdgree "滞后一期高管平均学历"
g lbage = l.ba
label variable lbage "滞后一期高管平均年龄"
g lbgender = l.bg
label variable lbgender "滞后一期高管女性占比"


* 模型构建～～～～～
* 一、同群效应存在性
* 1. 数据准备
** 企业投资水平
use "/Users/soyo/Downloads/毕业论文数据库/企业投资水平.dta"
bysort year industrycode : egen midinv=pctile(inv)
label variable midinv "公司所在行业的投资水平中位数"

** 资产负债率、总资产净利润率、账面市值比
label variable assetliabilityratio "资产负债率"
label variable roaa "总资产净利润率A"
rename assetliabilityratio Lev
destring sy, replace
rename symbol stkcd
gen year = substr(end,1,4)
destring year,replace
drop end
drop if year == .
label variable year "年份"
label variable stkcd "股票代码"
rename roaa ROA
xtset stkcd year
gen llev = l.Lev
label variable llev "滞后一期资产负债率"
gen lROA = l.ROA
label variable lROA "滞后一期总资产净利润率"

** 总资产、现金流
label variable bega10 "年初总资产"
label variable enda10 "年末总资产"
label variable CF "经营活动产生的现金流净额"
destring s, replace
gen year = substr(ac,1,4)
destring year,replace
drop ac
drop if year == .
label variable year "年份"
label variable stkcd "股票代码"
g CF = d100000/bega10
drop if stkcd == .
label variable CF "经营活动产生的现金流净额"
g asset = ln(enda10)
drop if asset == .
drop if CF == .
drop d100000 bega10 enda10
label variable asset "企业期末总资产的自然对数"
xtset stkcd year
gen lcf = l.CF
gen lasset = l.asset
label variable lcf "滞后一期现金流"
label variable lasset "滞后一期总资产"

** 货币资金、固定资产、上市年限、营业收入增长、TobinQ、第一大股东、董事会、性质
gen year = substr(accper,1,4)
destring year,replace
drop accper
destring stkcd, replace
xtset stkcd year
g cash = a001101000/a001000000
label variable cash "货币资金"
g fa = a001212000/a001000000
label variable fa "固定资产"
drop if year == .
gen lcash = l.cash
gen lfa = l.fa
gen lltime = l.ltime
gen lgrowth = l.f
label variable lgrowth "滞后一期企业营业收入增长率"
gen lfirst = l.largestholderrate
label variable lfirst "企业第一大股东持股比例"
gen ldual = l.y1001b
label variable ldual "滞后一期董事长与总经理兼任情况,1:是，0:否"
gen lboard = l.board
label variable lboard "滞后一期董事人数"
gen lindr = l.indr
label variable lindr "滞后一期独立董事占比"

** 数据文件合并
merge m:m stkcd year using "/Users/soyo/Downloads/毕业论文数据库/同群效应存在性控制变量.dta"
drop if year == 2009
drop if _merge != 3
drop _
drop CF asset Lev ROA
merge m:m stkcd year using "/Users/soyo/Downloads/毕业论文数据库/同群效应存在性检验.dta"

** 数据预处理
drop if _merge != 3
drop _merge
egen mis = rowmiss(_all)
tab mis 
drop if mis
drop mis //删除缺失值
winsor2 lcf lasset llev lROA inv midinv, replace cuts(1 99) trim
sum //描述性统计

* 2. 面板回归
xi:reg inv midinv lstate ldual lboard lindr lfirst ltobinQ lgrowth lcf lasset llev lROA lcash lfa lltime i.year i.industrycode
est store st_ols
esttab st_ols using 同群效应存在性.csv, replace star( * 0.10 ** 0.05 *** 0.01 ) nogaps compress ar2 obslast scalars(F)

* 二、同群效应对股价崩盘风险的影响
* 1. 数据准备
** 去重获得年度崩盘风险
sort stkcd year 
by stkcd year: gen yearcount = _n
drop if yearcount != 1
drop yearcount
save "/Users/soyo/Downloads/毕业论文数据库/崩盘风险检验.dta", replace

** 年度周平均收益率
gen year = substr(trddt,1,4)
destring year,replace
destring stkcd, replace
sort stkcd year
by stkcd year: gen weeknum = _n
by stkcd year: egen num = max(weeknum)
drop trddt
by stkcd year: egen mwrem = mean(wretwd)
label variable mwrem "个股年度平均周收益率"
rename mwrem Ret
sort stkcd year 
by stkcd year: gen yearcount = _n
drop if yearcount != 1
drop yearcount
tsset s y
g lret = l.R
drop Ret
merge m:m stkcd year using "/Users/soyo/Downloads/毕业论文数据库/崩盘风险检验.dta"
drop if _merge != 3
drop _
save "/Users/soyo/Downloads/毕业论文数据库/崩盘风险检验.dta", replace

** 年度周收益率的标准差
by stkcd year: egen std = sd(wretwd) 
sort stkcd year 
by stkcd year: gen yearcount = _n
drop if yearcount != 1
drop yearcount
tsset stk y
g lsigma = l.std
label variable lsigma "滞后一期年度周收益率的标准差"

** 操控性应计利润绝对值
g abse = abs(dis)
label variable abse "操控性应计额绝对值"
tsset s y
gen labse = l.ab
label variable labse "滞后一期操控性应计利润绝对值"
drop disacc abse

** 超额换手率
drop if to == .
gen year = substr(tr,1,4)
destring year,replace
destring stkcd, replace
sort stkcd year
by stkcd year: egen monthto = mean(toverosm)
label variable toverosm "月平均换手率"
by stkcd year: gen yearcount = _n
drop if yearcount != 1
drop yearcount
tsset s y
g lmot = l.mo
label variable lmot "t-1年月平均换手率"
g dturn = mo - lm
label variable dturn "月平均超额换手率"
drop if dturn == .
g ldturn = l.dturn
label variable ldturn "滞后一期月平均超额换手率"

* 2. 面板回归：同群效应与股价崩盘
egen mis = rowmiss(_all)
tab mis
drop if mis
drop if st == 1 //删除ST状态
drop st
winsor2 ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize herdinv DUVOL NCSKEW, replace cuts(1 99) trim
xi:reg NCSKEW herdinv ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.industrycode
est store st_ncsk
xi:reg DUVOL herdinv ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.industrycode
est store st_duvol
esttab st_ncsk st_duvol using 崩盘风险检验.csv, replace star( * 0.10 ** 0.05 *** 0.01 ) nogaps compress ar2 obslast scalars(F)

* 三、崩盘风险与高管特质
* 1. 数据准备
merge m:m stkcd year using "/Users/soyo/Downloads/毕业论文数据库/崩盘风险检验.dta"
drop if _merge != 3
drop _
save "/Users/soyo/Downloads/毕业论文数据库/高管特质与崩盘风险.dta"
drop if st == 1
drop st
winsor2 ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize herdinv DUVOL NCSKEW, replace cuts(1 99) trim

* 2.面板回归
gen herd_degree = herdinv*lbdgree
xi:reg NCSKEW herdinv herd_degree lbdgree ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.industrycode
est store NCS_degree
xi:reg DUVOL herdinv herd_degree lbdgree ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.industrycode
est store DUV_degree
gen herd_age = herdinv*lbage
xi:reg NCSKEW herdinv herd_age lbage ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.industrycode
est store NCS_age
xi:reg DUVOL herdinv herd_age lbage ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.industrycode
est store DUV_age
gen herd_gender = herdinv*lbgender
xi:reg NCSKEW herdinv herd_gender lbgender ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.industrycode
est store NCS_gender
xi:reg DUVOL herdinv herd_gender lbgender ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.industrycode
est store DUV_gender
esttab NCS_degree DUV_degree NCS_age DUV_age NCS_gender DUV_gender using 高管特质与崩盘风险.csv, replace star( * 0.10 ** 0.05 *** 0.01 ) nogaps compress ar2 obslast scalars(F)

* 四、稳健性检验

* 1.公司层面聚回归
* 生成行业哑变量编码
encode industrycode, gen (indst)
xtreg inv midinv lstate ldual lboard lindr lfirst ltobinQ lgrowth lcf lasset llev lROA lcash lfa lltime i.year i.indst, fe cluster(stkcd)
est store OLS_cluster
esttab OLS_cluster using 同群效应存在性聚回归.csv, replace star( * 0.10 ** 0.05 *** 0.01 ) nogaps compress ar2 obslast scalars(F)
* 崩盘风险检验
encode industrycode, gen (indst)
xtreg NCSKEW herdinv ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.indst, fe cluster(stkcd)
est store NCS_cluster
xtreg DUVOL herdinv ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.indst, fe cluster(stkcd)
est store DUVOL_cluster
esttab NCS_cluster DUVOL_cluster using 崩盘风险聚回归.csv, replace star( * 0.10 ** 0.05 *** 0.01 ) nogaps compress ar2 obslast scalars(F)
* 高管特征与崩盘风险
encode industrycode, gen (indst)
gen herd_degree = herdinv*lbdgree
xtreg NCSKEW herdinv herd_degree lbdgree ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.indst, fe cluster(stkcd)
est store NCS_degree
xtreg DUVOL herdinv herd_degree lbdgree ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.indst, fe cluster(stkcd)
est store DUV_degree
gen herd_age = herdinv*lbage
xtreg NCSKEW herdinv herd_age lbage ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.indst, fe cluster(stkcd)
est store NCS_age
xtreg DUVOL herdinv herd_age lbage ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.indst, fe cluster(stkcd)
est store DUV_age
gen herd_gender = herdinv*lbgender
xtreg NCSKEW herdinv herd_gender lbgender ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.indst, fe cluster(stkcd)
est store NCS_gender
xtreg DUVOL herdinv herd_gender lbgender ldturn labse lsigma lret lage lmb lgrowth llev lROA lsize i.year i.indst, fe cluster(stkcd)
est store DUV_gender
esttab NCS_degree DUV_degree NCS_age DUV_age NCS_gender DUV_gender using 高管特质崩盘风险聚回归.csv, replace star( * 0.10 ** 0.05 *** 0.01 ) nogaps compress ar2 obslast scalars(F)

* 2.工具变量
* 2.1 特质收益率计算
* 数据准备
label variable rf "无风险利率"
gen year = substr(cl,1,7)//在excel内针对重复值取平均数（暂时没想到更好的办法）
duplicates drop
sort year
gen monthcode = _n

* 系数回归
* ssc install rangestat
gen y = mretwd - rf
rangestat (reg) y ri sm hm um, interval(monthcode -36 -1) by(stkcd)

* 保留回归系数
gen jan = substr(year,6,7)
destring jan, replace
drop if jan != 1
gen fullyear = substr(year,1,4)
save "C:\Users\SAMSUNG\Desktop\梦里拿到本因坊\毕业论文数据库\特质收益回归系数.dta"
gen fullyear = substr(year,1,4)
merge m:m stkcd fullyear using "C:\Users\SAMSUNG\Desktop\梦里拿到本因坊\毕业论文数据库\特质收益回归系数.dta"
drop if _mer != 3
drop _

* 计算每个月特质收益率
gen exprr = b_cons + b_riskpremium1*riskpremium1 + b_smb1*smb1 + b_hml1*hml1 + b_umd1*umd1
drop if exprr == .
label variable exprr "每个月超额收益率的期望值"
gen n = y - exprr
label variable n "月股票特质收益率"

* 复合年度特质收益率
sort stkcd full
by stkcd fullyear: gen n1 = sum(n*mretwd)
by stkcd fullyear: gen n2 = sum(mretwd)
by stkcd fullyear: gen nyear = n1/n2
label variable nyear "年度股票特质收益率"
drop if nyear == .
save "C:\Users\SAMSUNG\Desktop\梦里拿到本因坊\毕业论文数据库\年度特质收益率.dta", replace
drop mretwd-rf y monthcode reg_nobs-n2

* 同行平均个股特质收益率
sort industrycode year
by industrycode year : egen nind = mean(nyear)
label variable nind "同行平均个股特质收益率"

* 工具变量
* ssc install xtivreg2
egen mis = rowmiss(_all)
tab mis 
drop if mis
drop mis
winsor2 lcf lasset llev lROA inv midinv, replace cuts(1 99) trim
xi:xtivreg inv  lstate ldual lboard lindr lfirst ltobinQ lgrowth lcf lasset llev lROA lcash lfa lltime i.year i.industrycode (midinv = nind)
est store m1
xi:xtivreg inv  lstate ldual lboard lindr lfirst ltobinQ lgrowth lcf lasset llev lROA lcash lfa lltime i.year i.industrycode (midinv = nind), fe
est store m2
ssc install outreg2
outreg2 [m1 m2] using xi_2sls.doc,replace tstat addtext(Company FE, YES )
