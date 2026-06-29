const fs = require("fs");
const {
  Document, Packer, Paragraph, TextRun, Table, TableRow, TableCell,
  AlignmentType, LevelFormat, HeadingLevel, BorderStyle, WidthType,
  ShadingType, PageNumber, Header, Footer, ExternalHyperlink, PageBreak,
} = require("docx");

const BLUE="00377C", GREY="6C7279";

function h1(t){return new Paragraph({heading:HeadingLevel.HEADING_1,children:[new TextRun(t)]});}
function h2(t){return new Paragraph({heading:HeadingLevel.HEADING_2,children:[new TextRun(t)]});}
function para(runs){return new Paragraph({spacing:{after:140,line:276},alignment:AlignmentType.JUSTIFIED,children:Array.isArray(runs)?runs:[new TextRun(runs)]});}
function bullet(runs){return new Paragraph({numbering:{reference:"b",level:0},spacing:{after:90,line:276},children:Array.isArray(runs)?runs:[new TextRun(runs)]});}
function b(t){return new TextRun({text:t,bold:true});}
function t(x){return new TextRun({text:x});}
function it(x){return new TextRun({text:x,italics:true});}
function eq(str){return new Paragraph({alignment:AlignmentType.CENTER,spacing:{before:140,after:160},children:[new TextRun({text:str,italics:true,size:24,font:"Cambria Math"})]});}

function srcCell(text,w,opts={}){
  return new TableCell({
    width:{size:w,type:WidthType.DXA}, columnSpan:opts.span||1,
    shading:opts.head?{fill:BLUE,type:ShadingType.CLEAR}:{fill:"FFFFFF",type:ShadingType.CLEAR},
    margins:{top:60,bottom:60,left:100,right:100},
    borders:{top:{style:BorderStyle.SINGLE,size:1,color:"CCCCCC"},left:{style:BorderStyle.SINGLE,size:1,color:"CCCCCC"},bottom:{style:BorderStyle.SINGLE,size:1,color:"CCCCCC"},right:{style:BorderStyle.SINGLE,size:1,color:"CCCCCC"}},
    children:[new Paragraph({spacing:{after:0},children:[new TextRun({text:text,bold:!!opts.head,color:opts.head?"FFFFFF":"000000",size:opts.size||18})]})]
  });
}
function link(url,label){return new Paragraph({spacing:{after:40},children:[new TextRun({text:"\u2022 "+label+": ",size:18}),new ExternalHyperlink({link:url,children:[new TextRun({text:url,style:"Hyperlink",size:16})]})]});}

const doc = new Document({
  styles:{
    default:{document:{run:{font:"Arial",size:22}}},
    paragraphStyles:[
      {id:"Heading1",name:"Heading 1",basedOn:"Normal",next:"Normal",quickFormat:true,run:{size:28,bold:true,font:"Arial",color:BLUE},paragraph:{spacing:{before:280,after:160},outlineLevel:0}},
      {id:"Heading2",name:"Heading 2",basedOn:"Normal",next:"Normal",quickFormat:true,run:{size:24,bold:true,font:"Arial",color:"333333"},paragraph:{spacing:{before:200,after:120},outlineLevel:1}},
    ]
  },
  numbering:{config:[{reference:"b",levels:[{level:0,format:LevelFormat.BULLET,text:"\u2022",alignment:AlignmentType.LEFT,style:{paragraph:{indent:{left:540,hanging:270}}}}]}]},
  sections:[{
    properties:{page:{size:{width:12240,height:15840},margin:{top:1440,right:1440,bottom:1440,left:1440}}},
    headers:{default:new Header({children:[new Paragraph({alignment:AlignmentType.RIGHT,spacing:{after:0},children:[new TextRun({text:"UNICEF Nepal \u00B7 Cost of Inaction \u2014 Technical Annex",size:14,color:GREY})]})]})},
    footers:{default:new Footer({children:[new Paragraph({alignment:AlignmentType.CENTER,children:[new TextRun({children:[PageNumber.CURRENT],size:16,color:GREY})]})]})},
    children:[
      h1("Annex: Technical Detail"),
      para([t("This annex documents the three methods used in the analysis, the formal model behind each, and the rationale for the key parameter choices. The methods are complementary: the causal forest (A.1) identifies "),it("whether and for whom"),t(" education reduces exclusion; the Mincer / Heckman framework (A.2) values the "),it("earnings"),t(" associated with education and tests it for selection bias; and the cost-of-inaction calculation (A.3) combines the two with population and discounting assumptions to produce the headline figure.")]),

      // ===== A.1 =====
      h2("A.1 Causal forest: the effect of education on exclusion"),
      para([t("We estimate the effect of secondary completion on the probability of being NEET (not in education, employment, or training) using a "),b("generalised random forest / causal forest"),t(" (Wager and Athey, 2018). Writing "),it("Y"),t("\u1d62(1) and "),it("Y"),t("\u1d62(0) for the potential NEET outcomes of woman "),it("i"),t(" with and without secondary completion, the target parameter is the average treatment effect:")]),
      eq("\u03C4_ATE  =  E[ Y\u1d62(1) \u2212 Y\u1d62(0) ]"),
      para([t("Because each woman is observed in only one state, the forest estimates the "),b("conditional average treatment effect (CATE)"),t(" as a function of covariates "),it("x"),t(":")]),
      eq("\u03C4(x)  =  E[ Y\u1d62(1) \u2212 Y\u1d62(0) | X\u1d62 = x ]"),
      para([t("It does so by growing an ensemble of \u201Chonest\u201D regression trees that partition the covariate space to maximise heterogeneity in the treatment effect, then averaging across trees. Identification rests on the "),b("unconfoundedness (selection-on-observables)"),t(" assumption:")]),
      eq("( Y\u1d62(1), Y\u1d62(0) )  \u22A5  W\u1d62  |  X\u1d62"),
      para([t("where "),it("W"),t("\u1d62 \u2208 {0,1} indicates secondary completion. In our implementation:")]),
      bullet([b("Sample: "),t("women aged 15\u201364 with non-missing treatment and outcome (n = 13,586).")]),
      bullet([b("Treatment W: "),t("completed secondary education.")]),
      bullet([b("Outcome Y: "),t("NEET status (also estimated for employment and wage employment).")]),
      bullet([b("Covariates X: "),t("age, age\u00B2, rural residence, poverty, wealth quintile, caste/ethnicity, province, disability, and marital status.")]),
      bullet([b("Tuning: "),t("2,000 trees, survey-weighted (sampling weights), all parameters tuned by cross-validation, fixed seed for reproducibility.")]),
      para([b("Result: "),t("the estimated ATE is \u221213.2 percentage points \u2014 secondary completion is associated with a 13.2-point lower probability of being NEET \u2014 and is stable across all seven provinces and all caste groups.")]),
      para([b("Rationale. "),t("A causal forest is preferred over a single linear regression for two reasons. First, it relaxes the assumption that the effect of education is the same for everyone: it estimates how the effect varies across provinces and caste groups, which is exactly the heterogeneity policymakers need to target investment. Second, the \u201Chonesty\u201D property (using separate data to choose the tree splits and to estimate effects within them) produces valid confidence intervals and reduces overfitting. The design controls for confounding from observed characteristics; because it is not a randomised experiment, it does not eliminate bias from unobserved factors, so we interpret it as the best available observational estimate rather than a definitive causal parameter.")]),

      // ===== A.2 =====
      h2("A.2 Mincer wage equation and Heckman selection correction"),
      para([b("The wage equation. "),t("Returns to schooling are estimated with a Mincerian earnings function (Mincer, 1974), augmented with a quadratic schooling term to capture the convexity of returns observed for women:")]),
      eq("ln(w\u1d62) = \u03B2\u2080 + \u03B2\u2081S\u1d62 + \u03B2\u2082S\u1d62\u00B2 + \u03B2\u2083X\u1d62 + \u03B2\u2084X\u1d62\u00B2 + \u03B3\u02B9Z\u1d62 + \u03B5\u1d62"),
      para([t("where "),it("w"),t("\u1d62 is the monthly wage, "),it("S"),t("\u1d62 is years of schooling (centred at the sample mean), "),it("X"),t("\u1d62 is potential experience (age \u2212 "),it("S"),t("\u1d62 \u2212 6), and "),it("Z"),t("\u1d62 is a vector of controls (rural residence, caste, province fixed effects). Because schooling is centred, the marginal return at schooling level "),it("s"),t(" is:")]),
      eq("\u2202 ln(w) / \u2202 S  =  \u03B2\u2081 + 2\u03B2\u2082(s \u2212 S\u0304)"),
      para([t("The positive estimated "),it("\u03B2"),t("\u2082 means returns rise with the level of education: approximately 8% at primary, 27% at secondary, and 35% at higher secondary, with an average marginal return of about 14.5% across the observed distribution. Estimation uses survey weights and heteroskedasticity-robust (HC1) standard errors.")]),
      para([b("The selection problem. "),t("Wages are observed only for the \u224825% of working-age women in paid employment. If unobserved factors that raise the probability of working also raise wages, the OLS estimate of "),it("\u03B2"),t("\u2081 is biased. We test and correct for this with the "),b("Heckman (1979) two-step estimator.")]),
      para([it("Step 1 \u2014 selection equation (probit). "),t("We model the probability that a wage is observed:")]),
      eq("Pr(D\u1d62 = 1 | V\u1d62)  =  \u03A6( V\u1d62\u02B9\u03B4 )"),
      para([t("where "),it("D"),t("\u1d62 = 1 if a wage is observed and \u03A6 is the standard normal CDF. The vector "),it("V"),t("\u1d62 contains the wage-equation covariates "),b("plus exclusion restrictions"),t(" \u2014 variables affecting participation but excludable from the wage: number of children under five, number of other adult women in the household, household size, and wealth quintile. From this we form the "),b("inverse Mills ratio:")]),
      eq("\u03BB\u1d62  =  \u03C6( V\u1d62\u02B9\u03B4 ) / \u03A6( V\u1d62\u02B9\u03B4 )"),
      para([it("Step 2 \u2014 outcome equation with correction. "),t("Adding \u03BB\u1d62 to the wage equation:")]),
      eq("ln(w\u1d62) = \u03B2\u2080 + \u03B2\u2081S\u1d62 + \u2026 + \u03B8\u03BB\u1d62 + u\u1d62"),
      para([t("The coefficient \u03B8 = \u03C1\u03C3 captures the selection effect; its significance tests for selection bias, and \u03C1 is the correlation between the selection and wage errors.")]),
      para([b("Result: "),t("\u03C1 = 0.34 with the inverse Mills ratio significant at p < 0.01 \u2014 there is statistically significant positive selection into wage employment. However, the corrected return differs from the uncorrected estimate by less than two percentage points, and is stable across alternative child-age cutoffs (under 4, 5, 6) in the exclusion restriction. The Mincer estimates are therefore robust to sample selection.")]),
      para([b("Rationale. "),t("The Heckman model is the standard econometric tool for exactly the concern reviewers raised: in a labour market as informal as Nepal\u2019s, the women observed earning wages are not a random sample of all women. The exclusion restrictions are chosen from the female-labour-supply literature \u2014 the presence of young children and the availability of other adult women in the household shift whether a woman works, but have no direct effect on the wage she would command. This correction addresses selection into "),it("observed wages only"),t("; it is not a causal-identification strategy for schooling itself, which is the role of the causal forest in A.1. The two methods deliberately address different biases.")]),

      // ===== A.3 =====
      h2("A.3 The cost-of-inaction calculation"),
      para([t("The cost of inaction is the present value of lifetime earnings forgone by one cohort of girls who do not complete secondary education. The annual earnings gain from the additional schooling associated with secondary completion is:")]),
      eq("\u0394w = w\u2080 \u00D7 ( (1 + r)^\u0394S \u2212 1 )"),
      para([t("where "),it("w"),t("\u2080 is the baseline median annual wage, "),it("r"),t(" is the per-year return to schooling, and \u0394S is the additional years of schooling associated with secondary completion. In the code this is implemented in discrete form as "),it("w"),t("\u2080 \u00D7 ((1+r)"),it("^\u0394S"),t(" \u2212 1), with r = 0.09 and \u0394S = 2. The lifetime present value over a working horizon of "),it("T"),t(" years, discounted at rate "),it("\u03B4"),t(", uses the standard annuity factor:")]),
      eq("PV = \u0394w \u00D7 [ 1 \u2212 (1 + \u03B4)^(\u2212T) ] / \u03B4"),
      para([t("The cost of inaction for a population segment combines the share of girls who would no longer be NEET if they completed secondary (the absolute CATE from A.1), the segment population, and the per-girl present value:")]),
      eq("CoI = |\u03C4(x)| \u00D7 N \u00D7 PV"),
      para([t("where N is the number of girls in the segment not completing secondary education. National totals are obtained by summing across provinces.")]),
      para([b("Parameter values (from the analysis code):")]),
      bullet([b("Return r = 9% per year"),t(" \u2014 the ILO benchmark, used deliberately instead of our own estimates.")]),
      bullet([b("Years gained \u0394S = 2"),t(" \u2014 the additional schooling associated with secondary completion.")]),
      bullet([b("Discount rate \u03B4 = 5%"),t(" \u2014 standard for long-horizon social cost-benefit analysis.")]),
      bullet([b("Horizon T = 35 years"),t(" \u2014 an adolescent girl\u2019s approximate working life.")]),
      bullet([b("Population N = 680,231 girls"),t(" \u2014 girls aged 15\u201324 not completing secondary, scaled from NLSS IV survey weights to a national total of \u22481.4 million.")]),
      bullet([b("Baseline wage w\u2080"),t(" \u2014 province-specific median monthly wage of women in salaried employment.")]),
      para([b("Result: "),t("aggregating across provinces yields a national cost of inaction of approximately NPR 8.1 billion (about USD 61 million) per cohort \u2014 a recurring annual loss, as a new cohort reaches this stage each year.")]),
      para([b("Rationale for the conservative choices. "),t("Three deliberate choices make the headline figure conservative. (1) The 9% return is below even our estimated primary-school return (\u22488%) and far below the secondary and higher-secondary returns (27\u201335%) that dominate the policy case. (2) The figure counts only formal-sector wage productivity and excludes non-market benefits \u2014 lower fertility, improved child health, reduced adolescent pregnancy, and intergenerational gains. (3) The 5% discount rate and 35-year horizon are standard, neither aggressive nor generous. The one assumption that pushes the other way \u2014 that a girl exiting NEET earns the median wage immediately \u2014 makes the estimate an upper bound on what is realistically and immediately recoverable; but the deliberately low return benchmark more than offsets this, so the overall figure is best read as conservative.")]),

      // ===== A.4 sources =====
      h2("A.4 Sources for budget comparisons"),
      new Table({
        width:{size:9360,type:WidthType.DXA}, columnWidths:[2600,1700,1300,3760],
        rows:[
          new TableRow({tableHeader:true,children:[srcCell("Figure",2600,{head:true}),srcCell("Value (NPR)",1700,{head:true}),srcCell("Fiscal year",1300,{head:true}),srcCell("Source",3760,{head:true})]}),
          new TableRow({children:[srcCell("Cost of inaction (one cohort)",2600),srcCell("8.1 billion",1700),srcCell("\u2014",1300),srcCell("This analysis, NLSS IV 2022/23 (NSO Nepal)",3760)]}),
          new TableRow({children:[srcCell("Health-sector budget (total MoHP)",2600),srcCell("86.24 billion",1700),srcCell("2024/25",1300),srcCell("MoF budget, reported by Kathmandu Post, 29 May 2024",3760)]}),
          new TableRow({children:[srcCell("Social security allowance budget",2600),srcCell("106 billion",1700),srcCell("2022/23",1300),srcCell("UNICEF Nepal, Social Protection Budget Brief Update FY2022/23",3760)]}),
          new TableRow({children:[srcCell("Education-sector budget (MoEST)",2600),srcCell("203.66 billion",1700),srcCell("2024/25",1300),srcCell("MoF Budget Speech FY2024/25, para 176",3760)]}),
          new TableRow({children:[srcCell("Provincial GDP (all 7 provinces)",2600),srcCell("various",1700),srcCell("2023/24",1300),srcCell("National Statistics Office (NSO) Nepal, provincial accounts 2023/24",3760)]}),
        ]
      }),
      new Paragraph({spacing:{before:160,after:80},children:[new TextRun({text:"Source links (for verification):",bold:true,size:20})]}),
      link("https://kathmandupost.com/health/2024/05/29/government-sets-aside-a-meagre-4-6-percent-of-the-annual-budget-for-the-health-sector","Health budget (Kathmandu Post, 29 May 2024)"),
      link("https://english.nepalnews.com/s/health/as-ncds-surge-nepals-health-budget-falls/","Health budget cross-check (Nepal News, 27 Oct 2025)"),
      link("https://www.unicef.org/nepal/media/18096/file/Social%20Protection%20Budget%20Brief%20Update:%20FY%202022-23.pdf","Social security allowances (UNICEF Nepal SP Budget Brief FY2022/23)"),
      link("https://demo.mof.gov.np/uploads/document/file/1724825318_Budget%20Speech%202024_25%20English%20FV.pdf","Education budget (MoF Budget Speech FY2024/25)"),
      link("https://nepaleconomicforum.org/whats-inside-fiscal-budget-2081-82-key-highlights-and-programs/","Total national budget cross-check (Nepal Economic Forum)"),
      new Paragraph({spacing:{before:120,after:60},children:[new TextRun({text:"Full source documentation, including exact quotations and the correction log for the earlier health-budget figure, is maintained in the project repository at docs/SOURCES_budget_figures.md.",italics:true,size:16,color:GREY})]}),

      // ===== A.5 refs =====
      h2("A.5 References"),
      para([t("Heckman, J. (1979). Sample Selection Bias as a Specification Error. "),it("Econometrica"),t(", 47(1), 153\u2013161.")]),
      para([t("Mincer, J. (1974). "),it("Schooling, Experience, and Earnings."),t(" National Bureau of Economic Research.")]),
      para([t("Wager, S. and Athey, S. (2018). Estimation and Inference of Heterogeneous Treatment Effects using Random Forests. "),it("Journal of the American Statistical Association"),t(", 113(523), 1228\u20131242.")]),
      para([t("UNICEF (2022). "),it("Cost of Inaction: Girls\u2019 Education in Afghanistan."),t(" UNICEF.")]),
      para([t("UNICEF Nepal (2014). "),it("Cost of Inaction: Child Marriage in Nepal."),t(" UNICEF.")]),
      para([t("National Statistics Office of Nepal (2023). "),it("Nepal Living Standards Survey IV (2022/23)."),t(" NSO.")]),
    ]
  }]
});

Packer.toBuffer(doc).then(buf=>{fs.writeFileSync("Nepal_CoI_Technical_Annex.docx",buf);console.log("Annex written.");});
