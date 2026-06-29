// =============================================================================
// generate_report.js — Nepal Cost of Inaction Policy Report (Word .docx)
// Run from the analysis/ project root:  node generate_report.js
// Reads figures from outputs/figures/ and writes reports/Nepal_CoI_Policy_Report.docx
// =============================================================================
const fs = require("fs");
const path = require("path");
const {
  Document, Packer, Paragraph, TextRun, Table, TableRow, TableCell,
  ImageRun, Header, Footer, AlignmentType, LevelFormat, HeadingLevel,
  BorderStyle, WidthType, ShadingType, VerticalAlign, PageNumber, PageBreak,
  TableOfContents, ExternalHyperlink
} = require("docx");

const FIG = "outputs/figures";
const UNICEF_BLUE = "00377C";
const UNICEF_LIGHT = "1CABE2";
const GREY = "6C7279";

// Helper: embed an image if it exists, else a labelled placeholder paragraph
function figure(file, widthIn, caption) {
  const p = path.join(FIG, file);
  const children = [];
  if (fs.existsSync(p)) {
    const data = fs.readFileSync(p);
    // assume 2:1-ish aspect; width in inches -> EMU (914400/inch)
    const w = Math.round(widthIn * 914400);
    const h = Math.round(w * 0.5);
    children.push(new Paragraph({
      alignment: AlignmentType.CENTER,
      spacing: { before: 120, after: 60 },
      children: [new ImageRun({ type: "png", data, transformation: { width: widthIn * 96, height: widthIn * 96 * 0.5 } })]
    }));
  } else {
    children.push(new Paragraph({
      alignment: AlignmentType.CENTER,
      spacing: { before: 120, after: 60 },
      border: { top: {style:BorderStyle.SINGLE,size:4,color:"CCCCCC"}, bottom:{style:BorderStyle.SINGLE,size:4,color:"CCCCCC"}, left:{style:BorderStyle.SINGLE,size:4,color:"CCCCCC"}, right:{style:BorderStyle.SINGLE,size:4,color:"CCCCCC"} },
      children: [new TextRun({ text: `[ Figure: ${file} — not found at ${p} ]`, italics: true, color: "999999", size: 18 })]
    }));
  }
  children.push(new Paragraph({
    alignment: AlignmentType.CENTER,
    spacing: { after: 200 },
    children: [new TextRun({ text: caption, italics: true, size: 16, color: GREY })]
  }));
  return children;
}

function h1(t){return new Paragraph({heading:HeadingLevel.HEADING_1,children:[new TextRun(t)]});}
function h2(t){return new Paragraph({heading:HeadingLevel.HEADING_2,children:[new TextRun(t)]});}
function p(runs, opts={}){return new Paragraph({spacing:{after:140,line:276},alignment:opts.align||AlignmentType.JUSTIFIED,children:Array.isArray(runs)?runs:[new TextRun(runs)]});}
function bullet(runs){return new Paragraph({numbering:{reference:"bullets",level:0},spacing:{after:80,line:276},children:Array.isArray(runs)?runs:[new TextRun(runs)]});}
function numbered(runs,ref="numbers"){return new Paragraph({numbering:{reference:ref,level:0},spacing:{after:100,line:276},children:Array.isArray(runs)?runs:[new TextRun(runs)]});}
function b(t){return new TextRun({text:t,bold:true});}
function tr(t){return new TextRun({text:t});}
function it(t){return new TextRun({text:t,italics:true});}

// Source-table helper
function srcCell(text, w, opts={}) {
  return new TableCell({
    width:{size:w,type:WidthType.DXA},
    columnSpan: opts.span||1,
    shading: opts.head?{fill:UNICEF_BLUE,type:ShadingType.CLEAR}:{fill:"FFFFFF",type:ShadingType.CLEAR},
    margins:{top:60,bottom:60,left:100,right:100},
    borders:{top:{style:BorderStyle.SINGLE,size:1,color:"CCCCCC"},bottom:{style:BorderStyle.SINGLE,size:1,color:"CCCCCC"},left:{style:BorderStyle.SINGLE,size:1,color:"CCCCCC"},right:{style:BorderStyle.SINGLE,size:1,color:"CCCCCC"}},
    children:[new Paragraph({spacing:{after:0},children:[new TextRun({text:text,bold:!!opts.head,color:opts.head?"FFFFFF":"000000",size:opts.size||18})]})]
  });
}

const doc = new Document({
  styles: {
    default: { document: { run: { font: "Arial", size: 22 } } },
    paragraphStyles: [
      { id:"Heading1", name:"Heading 1", basedOn:"Normal", next:"Normal", quickFormat:true,
        run:{size:28,bold:true,font:"Arial",color:UNICEF_BLUE},
        paragraph:{spacing:{before:280,after:160},outlineLevel:0} },
      { id:"Heading2", name:"Heading 2", basedOn:"Normal", next:"Normal", quickFormat:true,
        run:{size:24,bold:true,font:"Arial",color:"333333"},
        paragraph:{spacing:{before:200,after:120},outlineLevel:1} },
      { id:"Title", name:"Title", basedOn:"Normal", next:"Normal",
        run:{size:40,bold:true,font:"Arial",color:UNICEF_BLUE},
        paragraph:{spacing:{after:120}} },
    ]
  },
  numbering: {
    config: [
      { reference:"bullets", levels:[{level:0,format:LevelFormat.BULLET,text:"\u2022",alignment:AlignmentType.LEFT,style:{paragraph:{indent:{left:540,hanging:270}}}}] },
      { reference:"numbers", levels:[{level:0,format:LevelFormat.DECIMAL,text:"%1.",alignment:AlignmentType.LEFT,style:{paragraph:{indent:{left:540,hanging:270}}}}] },
      { reference:"recs", levels:[{level:0,format:LevelFormat.DECIMAL,text:"%1.",alignment:AlignmentType.LEFT,style:{paragraph:{indent:{left:540,hanging:270}}}}] },
    ]
  },
  sections: [{
    properties: { page: { size:{width:12240,height:15840}, margin:{top:1440,right:1440,bottom:1440,left:1440} } },
    headers: { default: new Header({ children:[ new Paragraph({ alignment:AlignmentType.RIGHT, spacing:{after:0}, children:[new TextRun({text:"UNICEF Nepal \u00B7 Cost of Inaction \u2014 Adolescent Girls",size:14,color:GREY})] }) ] }) },
    footers: { default: new Footer({ children:[ new Paragraph({ alignment:AlignmentType.CENTER, children:[new TextRun({text:"",size:14,color:GREY}), new TextRun({children:[PageNumber.CURRENT],size:16,color:GREY})] }) ] }) },
    children: [
      // ========================= TITLE =========================
      new Paragraph({ spacing:{before:1200,after:60}, children:[new TextRun({text:"The Cost of Inaction",size:48,bold:true,color:UNICEF_BLUE})] }),
      new Paragraph({ spacing:{after:200}, children:[new TextRun({text:"Adolescent Girls\u2019 Education and Human Capital in Nepal",size:32,bold:true,color:"333333"})] }),
      new Paragraph({ spacing:{after:60}, children:[new TextRun({text:"A policy analysis using the Nepal Living Standards Survey IV (2022/23)",size:22,italics:true,color:GREY})] }),
      new Paragraph({ spacing:{before:400,after:40}, children:[new TextRun({text:"Hernando Grueso, PhD",size:22,bold:true})] }),
      new Paragraph({ spacing:{after:40}, children:[new TextRun({text:"UNICEF",size:20})] }),
      new Paragraph({ spacing:{after:40}, children:[new TextRun({text:"May 2026",size:20,color:GREY})] }),
      new Paragraph({ children:[new PageBreak()] }),

      // ========================= EXEC SUMMARY =========================
      h1("Executive Summary"),
      p([
        tr("Nepal has made remarkable progress in getting girls into school. Today, adolescent girls are enrolled at higher rates than boys (40.2 per cent versus 34.8 per cent). Yet this progress in the classroom does not carry through to the labour market. Nearly half of adolescent girls aged 15\u201324 \u2014 "),
        b("46.8 per cent"),
        tr(" \u2014 are not in education, employment, or training (NEET), almost double the rate for boys (26.4 per cent). The gender gap in Nepal is no longer primarily a problem of school access; it is a problem of the "),
        b("transition from school to economic opportunity."),
      ]),
      p([
        tr("This analysis quantifies the economic cost of that lost transition. Using nationally representative microdata from the Nepal Living Standards Survey IV (NLSS IV, 2022/23) and a combination of causal machine-learning and wage-equation methods, we estimate that Nepal forgoes approximately "),
        b("NPR 8.1 billion (about USD 61 million)"),
        tr(" in lifetime productivity for every single cohort of adolescent girls who do not complete secondary education. A new cohort reaches this stage every year, so this is a recurring annual loss, not a one-off."),
      ]),
      p([ tr("To place that figure in perspective, it is equivalent to roughly "), b("9.4 per cent of Nepal\u2019s annual health-sector budget"), tr(", "), b("7.6 per cent of the social security allowance budget"), tr(", and "), b("4.0 per cent of the education-sector budget."), ]),
      p([ b("Three findings stand out for policy:") ]),
      numbered([ b("Child marriage is the single most powerful driver of exclusion. "), tr("Being currently married is associated with a 61-percentage-point fall in enrolment and a 42-point rise in NEET \u2014 larger than the combined association of poverty, caste, and disability. The share of girls who are married jumps from 10 per cent at ages 15\u201318 to 56 per cent at ages 19\u201324.") ]),
      numbered([ b("Secondary education has a robust, stable effect on inclusion. "), tr("Completing secondary school is associated with a 13.2-percentage-point reduction in the probability of being NEET, and this effect is stable across all seven provinces and all caste groups.") ]),
      numbered([ b("The returns to schooling rise steeply with each level. "), tr("Each additional year of schooling is associated with a 14.5 per cent average increase in earnings for girls, and these returns are convex \u2014 rising from about 8 per cent at primary to 27 per cent at secondary and 35 per cent at higher secondary.") ]),
      p([ tr("The policy implication is that investments should reach girls "), b("before"), tr(" the post-18 transition, reward "), b("every year of schooling retained"), tr(", and be paired with measures that create real economic opportunity for young women.") ]),
      new Paragraph({ children:[new PageBreak()] }),

      // ========================= 1. MOTIVATION =========================
      h1("1. Introduction and Motivation"),
      p("Investing in girls\u2019 education is among the most widely-evidenced development priorities in the world. The case is usually made in terms of rights and long-run social benefits. This report makes the case in the language of public finance: what does Nepal lose, in measurable economic terms, when adolescent girls do not complete secondary education and do not transition into productive economic activity?"),
      p("The motivation is both moral and fiscal. Nepal\u2019s Constitution guarantees education as a fundamental right, and the country has committed to the Sustainable Development Goals. But for a Ministry of Finance weighing competing claims on a constrained budget, the relevant question is concrete: is closing the gender gap in the school-to-work transition an expenditure, or an investment with a positive return? This analysis is designed to answer that question with defensible numbers."),
      p([ tr("This work follows the methodology of UNICEF\u2019s cost-of-inaction analyses in other contexts \u2014 notably the "), it("Cost of Inaction: Girls\u2019 Education in Afghanistan"), tr(" (2022) and Nepal\u2019s own "), it("Cost of Inaction of Child Marriage"), tr(" (2014) \u2014 while adapting the approach to Nepal\u2019s current data and using more recent causal-inference techniques.") ]),

      // ========================= 2. DATA =========================
      h1("2. Data"),
      p([ tr("The analysis uses the "), b("Nepal Living Standards Survey IV (NLSS IV), 2022/23"), tr(", produced by the National Statistics Office (NSO) of Nepal. The NLSS IV is a nationally representative, two-stage stratified household survey covering all seven provinces. It interviewed 46,870 individuals in 9,600 households.") ]),
      p([ tr("The core analytical sample is "), b("9,785 youth aged 15\u201324"), tr(" (4,919 girls and 4,866 boys). For the wage and causal-effect analyses we use the broader sample of "), b("13,586 women aged 15\u201364."), tr(" All estimates use the survey\u2019s sampling weights and account for its complex design (primary sampling units and provincial strata).") ]),
      p([ b("Key variables"), tr(" include school enrolment and attainment (NLSS education module), employment and NEET status (defined using the ILO seven-day reference period), monthly wages, marital status, caste and ethnicity (grouped as Brahmin/Chhetri, Janajati, Dalit, and Religious/Linguistic minorities), disability status (Washington Group questions), wealth quintile, and province of residence.") ]),

      // ========================= 3. METHODOLOGY =========================
      h1("3. Methodology and Key Assumptions"),
      p("The analysis combines three established methods. Full technical detail is provided in the Annex; the summary here is intended for a non-specialist reader."),
      h2("3.1 Measuring the gaps"),
      p("We first document the size and structure of the gender gap using descriptive statistics and a multivariate (linear probability) regression that isolates the independent association of each factor \u2014 marriage, caste, wealth, disability, geography \u2014 with enrolment and NEET status."),
      h2("3.2 Estimating the causal effect of education"),
      p([ tr("To estimate the effect of secondary education on the probability of being NEET, we use a "), b("causal forest"), tr(" (a machine-learning method for estimating causal effects, Wager and Athey 2018). This approach allows the effect of education to vary across individuals and groups, and provides a credible average treatment effect that is more robust than a simple correlation.") ]),
      h2("3.3 Valuing the lost earnings"),
      p([ tr("To translate education into economic value, we estimate a "), b("Mincer wage equation"), tr(" \u2014 the standard tool in labour economics relating wages to years of schooling and experience. Because wages are observed only for women in paid employment (about 25 per cent of working-age women), we test for selection bias using a "), b("Heckman selection model."), tr(" The selection term is statistically significant but changes the estimated return by less than two percentage points, confirming that the wage estimates are robust (see Annex).") ]),
      h2("3.4 Key assumptions and limitations"),
      p("Three assumptions are central to interpreting the cost-of-inaction figure, and we state them plainly so that the estimate is not over-read:"),
      bullet([ b("Formal-sector focus. "), tr("Wage returns are estimated on women in paid employment, who are a minority of all working-age women. The analysis captures the economic-productivity loss associated with formal labour-market engagement; it does not value unpaid or subsistence work.") ]),
      bullet([ b("Labour-market entry. "), tr("The break-even and cost figures assume that a girl who completes secondary education enters the labour market and earns the median wage. In reality not all will, so the figures describe potential economic value, not a guaranteed return.") ]),
      bullet([ b("An upper bound, not a lower bound. "), tr("Because the estimate assumes a girl who exits NEET earns the median wage immediately \u2014 with no friction from job search, childcare, mobility, or social norms \u2014 it should be read as an "), b("upper bound"), tr(" on what is realistically recoverable, not a floor.") ]),
      p([ tr("We also use a deliberately "), b("conservative wage-return benchmark of 9 per cent per year of schooling"), tr(" (the International Labour Organization\u2019s standard benchmark) for the headline cost calculation, rather than our own higher estimates. As the Annex shows, this 9 per cent figure sits "), b("below even the primary-school return"), tr(" we estimate, which makes the resulting cost a conservative one.") ]),
      new Paragraph({ children:[new PageBreak()] }),

      // ========================= 4. FINDINGS =========================
      h1("4. Findings"),
      h2("4.1 The gap is in the labour market, not the classroom"),
      p("Girls in Nepal now enrol in school at higher rates than boys, but are far more likely to be excluded from both school and work as they reach adulthood. The female NEET rate of 46.8 per cent is nearly double the male rate, and the gap widens with age \u2014 from about 9 percentage points at ages 15\u201318 to almost 25 points at ages 19\u201324."),
      ...figure("d1_overall_gap.png", 5.8, "Figure 1. Enrolment, employment, and NEET by sex, youth aged 15\u201324. Source: Author\u2019s calculations, NLSS IV 2022/23."),
      ...figure("d4_gap_by_age.png", 6.0, "Figure 2. Youth outcomes by age group and sex. The NEET gap widens sharply after age 18. Source: Author\u2019s calculations, NLSS IV 2022/23."),

      h2("4.2 Child marriage is the strongest driver of exclusion"),
      p("Among all the factors we examine, marital status has by far the largest association with girls\u2019 exclusion. Being currently married is associated with a 61-percentage-point reduction in enrolment and a 42-point increase in NEET, after controlling for poverty, caste, geography, and disability. This pattern holds across every caste group."),
      p("Crucially, the prevalence of marriage changes dramatically across the adolescent years: 10 per cent of girls aged 15\u201318 are married, rising to 56 per cent by ages 19\u201324. The divergence in outcomes happens at this transition \u2014 which is why interventions need to reach girls before they reach it."),
      ...figure("d3_intersectionality.png", 6.0, "Figure 3. NEET rate by marital status and caste, girls 15\u201324. Marriage is associated with near-universal NEET across all groups. Source: Author\u2019s calculations, NLSS IV 2022/23."),
      ...figure("d3b_enrolment_intersectionality.png", 6.0, "Figure 4. Enrolment by marital status and caste, girls 15\u201324. Source: Author\u2019s calculations, NLSS IV 2022/23."),

      h2("4.3 Education works \u2014 everywhere"),
      p("The causal-forest analysis shows that completing secondary education is associated with a 13.2-percentage-point reduction in the probability of being NEET for women aged 15\u201364. This effect is remarkably stable: it holds across all seven provinces and all caste groups, indicating that education is a reliable lever for inclusion regardless of where a girl lives or which community she belongs to."),
      ...figure("r2_cate_caste.png", 5.8, "Figure 5. Estimated effect of secondary completion on NEET probability, by caste. Source: Author\u2019s calculations, NLSS IV 2022/23."),

      h2("4.4 The returns to schooling rise with each level"),
      p("Each additional year of schooling is associated with a 14.5 per cent average increase in girls\u2019 lifetime earnings. Critically, these returns are convex \u2014 they rise with the level of education, from roughly 8 per cent at primary, to 27 per cent at secondary, to 35 per cent at higher secondary. This means the economic loss from a girl leaving school is greatest precisely at the upper-secondary level, where Nepal\u2019s gender gap in the school-to-work transition is widening."),
      ...figure("m1_mincer_lifetime.png", 5.8, "Figure 6. Projected lifetime earnings by years of schooling and sex. Source: Author\u2019s calculations, NLSS IV 2022/23."),

      h2("4.5 The cost of inaction"),
      p([ tr("Combining the causal effect of education, the conservative wage return, and the size of the affected population (680,231 girls), we estimate that Nepal forgoes approximately "), b("NPR 8.1 billion (about USD 61 million)"), tr(" in lifetime productivity for each cohort of adolescent girls who do not complete secondary education.") ]),
      ...figure("c3_coi_vs_budget.png", 6.2, "Figure 7. The annual cost of inaction compared with national budget lines. Sources: This analysis (NLSS IV 2022/23); health budget MoHP FY2024/25; social security allowances UNICEF Nepal FY2022/23; education budget MoF FY2024/25. See source notes."),
      p([ tr("This figure is equivalent to about 9.4 per cent of the annual health-sector budget, 7.6 per cent of the social security allowance budget, and 4.0 per cent of the education budget. It excludes the substantial non-market benefits of girls\u2019 education \u2014 lower fertility, better child health, reduced adolescent pregnancy, and intergenerational gains \u2014 and so understates the true cost.") ]),
      new Paragraph({ children:[new PageBreak()] }),

      // ========================= 5. RECOMMENDATIONS =========================
      h1("5. Policy Recommendations"),
      p("The findings point to four priority areas for investment. They are complementary: education alone is necessary but not sufficient, because the binding constraints \u2014 marriage and the absence of economic opportunity \u2014 sit outside the classroom."),
      numbered([ b("Education \u2014 universal secondary completion with year-by-year retention. "), tr("Because returns are convex, programmes should reward every year of schooling retained, with the largest gains concentrated at upper-secondary and tertiary level. Conditional support should target retention through the post-18 transition.") ], "recs"),
      numbered([ b("Child marriage \u2014 treat prevention as an economic priority. "), tr("Marriage is the single largest driver of girls\u2019 exclusion, and its prevalence rises sharply after age 18. Enforcement of existing legislation, concentrated in the provinces with the widest gaps (Madhesh and Lumbini), should reach girls before the transition.") ], "recs"),
      numbered([ b("Livelihoods \u2014 bridge programmes, vocational training, and economic opportunity. "), tr("Because returns are concentrated in formal wage work, which absorbs only a minority of women, education must be paired with demand-side measures: skills bridges from school to work, vocational training, and second-chance pathways for those who have already left.") ], "recs"),
      numbered([ b("Equity \u2014 close the gaps facing Dalit girls. "), tr("Education alone leaves Dalit women with a residual labour-market disadvantage. Pair education investment with apprenticeships, targeted scholarships, and anti-discrimination measures.") ], "recs"),
      p([ tr("Taken together, these investments are strongly cost-effective. Any secondary-completion programme costing less than the per-girl break-even \u2014 on the order of USD 90 in cumulative lifetime social spending \u2014 would yield a positive economic return in pure productivity terms, before counting the substantial health, fertility, and intergenerational benefits that this analysis does not monetise.") ]),
      new Paragraph({ children:[new PageBreak()] }),

      // ========================= ANNEX =========================
      h1("Annex: Technical Detail"),
      h2("A.1 The causal-forest estimate"),
      p("The effect of secondary education on NEET status is estimated using a generalised random forest (Wager and Athey 2018) on the sample of women aged 15\u201364 (n = 13,586). The treatment is secondary completion; the outcome is NEET status; controls include age, marriage, caste, wealth, disability, and province. The estimated average treatment effect is a reduction of 13.2 percentage points in NEET probability, stable across provinces and caste groups. This design addresses selection on observable characteristics; it is not a randomised experiment, and residual unobserved confounding cannot be fully excluded."),
      h2("A.2 The Mincer wage equation and Heckman selection model"),
      p("Wage returns are estimated from a Mincer equation regressing log monthly wages on years of schooling (centred, with a quadratic term), experience, experience squared, and controls for rural residence, caste, and province, using survey weights and heteroskedasticity-robust (HC1) standard errors. The quadratic term captures the convexity of returns."),
      p("Because wages are observed only for the approximately 25 per cent of working-age women in paid employment, we test for sample-selection bias using a Heckman (1979) two-step model. The selection equation models the probability of being observed in the wage sample, with exclusion restrictions \u2014 the number of children under five in the household, the number of other adult women, household size, and wealth quintile \u2014 that plausibly affect labour-market participation but not the wage itself."),
      p([ b("Result: "), tr("the selection term is statistically significant (correlation \u03C1 = 0.34; inverse Mills ratio p < 0.01), confirming positive selection into wage work. However, correcting for selection shifts the estimated return by less than two percentage points. The Mincer estimates are therefore robust to selection. This addresses only selection into observed wages; it is not a causal-identification claim for schooling, which rests on the causal-forest design above.") ]),
      ...figure("n1_heckman_returns.png", 6.0, "Figure A1. Average marginal return to schooling: OLS versus Heckman selection-corrected estimates, with robustness across exclusion-restriction definitions. Source: Author\u2019s calculations, NLSS IV 2022/23."),
      h2("A.3 The cost-of-inaction calculation"),
      p("The cost of inaction combines four elements: (i) the causal effect of education on labour-market inclusion; (ii) a conservative 9 per cent per-year wage return (the ILO benchmark, below our own estimated returns); (iii) the median wage of women in salaried employment; and (iv) the affected population of 680,231 girls. Lifetime earnings are discounted at 5 per cent over a 35-year working horizon. The resulting figure of NPR 8.1 billion is the present value of lifetime productivity forgone by one cohort."),

      h2("A.4 Sources for budget comparisons"),
      new Table({
        width:{size:9360,type:WidthType.DXA},
        columnWidths:[2600,1700,1300,3760],
        rows:[
          new TableRow({children:[srcCell("Figure",2600,{head:true}),srcCell("Value (NPR)",1700,{head:true}),srcCell("Fiscal year",1300,{head:true}),srcCell("Source",3760,{head:true})]}),
          new TableRow({children:[srcCell("Cost of inaction (one cohort)",2600),srcCell("8.1 billion",1700),srcCell("\u2014",1300),srcCell("This analysis, NLSS IV 2022/23 (NSO Nepal)",3760)]}),
          new TableRow({children:[srcCell("Health-sector budget (total MoHP)",2600),srcCell("86.24 billion",1700),srcCell("2024/25",1300),srcCell("Ministry of Finance, reported by Kathmandu Post, 29 May 2024",3760)]}),
          new TableRow({children:[srcCell("Social security allowance budget",2600),srcCell("106 billion",1700),srcCell("2022/23",1300),srcCell("UNICEF Nepal, Social Protection Budget Brief Update FY2022/23",3760)]}),
          new TableRow({children:[srcCell("Education-sector budget (MoEST)",2600),srcCell("203.66 billion",1700),srcCell("2024/25",1300),srcCell("MoF Budget Speech FY2024/25, para 176",3760)]}),
        ]
      }),
      new Paragraph({ spacing:{before:120,after:60}, children:[new TextRun({text:"Full source links are maintained in the project documentation (docs/SOURCES_budget_figures.md).",italics:true,size:16,color:GREY})] }),

      h2("A.5 References"),
      p([ tr("Wager, S. and Athey, S. (2018). Estimation and Inference of Heterogeneous Treatment Effects using Random Forests. "), it("Journal of the American Statistical Association"), tr(", 113(523).") ]),
      p([ tr("Heckman, J. (1979). Sample Selection Bias as a Specification Error. "), it("Econometrica"), tr(", 47(1).") ]),
      p([ tr("UNICEF (2022). "), it("Cost of Inaction: Girls\u2019 Education in Afghanistan."), tr(" UNICEF.") ]),
      p([ tr("UNICEF Nepal (2014). "), it("Cost of Inaction: Child Marriage in Nepal."), tr(" UNICEF.") ]),
      p([ tr("National Statistics Office of Nepal (2023). "), it("Nepal Living Standards Survey IV (2022/23)."), ]),
    ]
  }]
});

Packer.toBuffer(doc).then(buf => {
  const out = "reports/Nepal_CoI_Policy_Report.docx";
  fs.writeFileSync(out, buf);
  console.log("Report written to " + out);
});
