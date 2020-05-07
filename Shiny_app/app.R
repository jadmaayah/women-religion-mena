library(shiny)
library(tidyverse)
library(janitor)
library(shinythemes)
library(broom)

culture <- read_rds("culture.rds")
elect <- read_rds("elect.rds")
rlg_clt <- read_rds("rlg_clt.rds")
rlg_clt_plot <- read_rds("rlgclt_plot.rds")
rlg_cltplot <- read_rds("rlgclt_plot.rds")
rlg_clt_cor <- read_rds("rlgclt_cor.rds")
rlg_clt_reg <- read_rds("rlgclt_reg.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),
                navbarPage(
                    "Women and Religion in the Middle East",
                    tabPanel(
                        "Policy Stances and Cultural Outlooks",
                        htmlOutput("text1"),
                        selectInput("country", h3("Select Country"), 
                                    c("Algeria" = 1, 
                                      "Egypt" = 5,
                                      "Iraq" = 7,
                                      "Jordan" = 8,
                                      "Lebanon" = 10,
                                      "Libya" = 11,
                                      "Morocco" = 13,
                                      "Palestine" = 15,
                                      "Sudan" = 19,
                                      "Tunisia" = 21,
                                      "Yemen" = 22)),
                        plotOutput("policy_plot"),
                        htmlOutput("text2"),
                        selectInput("question", h3("Select Prompt"), 
                                c("Head of State" = "q601_1", 
                                  "Political Leadership" = "q601_3",
                                  "Higher Education" = "q601_4",
                                  "Traveling Abroad" = "q601_7",
                                  "Divorce" = "q601_14",
                                  "Final Say in Family Matters" = "q601_18",
                                  "Inheritance" = "q601_9")),
                        plotOutput("culture_plot"),
                        htmlOutput("text3"),
                        plotOutput("rlg_clt_plot"),
                        htmlOutput("text4")),
                    
                    tabPanel("Models",
                             htmlOutput("text5"),
                             plotOutput("rlgclt_plot"),
                             htmlOutput("text6"),
                             verbatimTextOutput("rlgclt_cor"),
                             htmlOutput("text7"),
                             verbatimTextOutput("rlgclt_reg")),
                    
                    tabPanel("About",
                             htmlOutput("about"))))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$text1 <- renderUI({
    HTML("<b> <h2> The Position of Women in the Middle East</h2> </b>
    <p> <a href=https://ips-dc.org/women_in_the_middle_east>The Institute for Policy Studies</a>
    stipulates that the problems of Middle Eastern women are acute. The think 
    tank points to Islamic and Christian jurists and theologists – “all of them 
    males” – as having turned Middle Eastern society exclusivist and conservative, 
    thus burdening women. Even as the focus of analysis is shifted to public life, scholars argue that
    top positions in government are denied to women based on dubious interpretations 
    of religious texts: “the political representation of women in parliaments in 
    Arab nations lags behind all other countries of the world.” On the other hand, 
    Islamic clerics continue to enjoy power on both a cultural and a political level. 
    
    <h3> <b> Support for Women in the Government</h3> </b> 
    
    <p> While the role of women in Arab governments is limited, it is important to 
    ask whether these realities match up with public opinion. Should the public 
    agree that women deserve fairer representation, then the grounds for exclusivist 
    policies can surely unravel. The following graph looks at the support for fairer 
    government representation for Arab women, grouped by country.")
  })
  
    output$policy_plot <- renderPlot({
        elect %>% 
            filter(country == input$country) %>% 
            ggplot(aes(x = factor(q601a), y = ..count../sum(..count..), fill = q601a)) +
            geom_bar(show.legend = FALSE) +
            labs(title = "Women in the Government",
                 subtitle = "Some people think in order to achieve fairer representation a certain percentage of elected positions should be set aside for women. 
                 To what extent do you agree with this statement?",
                 caption = "Data from the Arab Barometer (2018-2019)",
                 x = "Level of Agreement",
                 y = "Percentage of Respondents") +
            scale_y_continuous(labels = scales::percent) +
            scale_x_discrete(labels = c("1" = "strongly agree", 
                                        "2" = "agree", 
                                        "3" = "disagree",
                                        "4" = "strongly disagree")) +
            theme_classic()
    })
    
    output$text2 <- renderUI({
      HTML("As shown in the data, every single country in question has overwhelming 
      support for fairer government representation for women. It is clear, on a 
      country-by-country basis, that the Middle East contains publics predominantly 
      supportive of fairer, non-discriminatory electoral practices. In relation to 
      the question at hand, it is evident that a shift towards fairer representation 
      for women in government holds public support, although imperfect.
      
      <p><p>
      However, this is merely a policy stance. The Institute of Policy Studies argues 
      that “in reality, the role of culture has been […] prominent in perpetuating the 
      oppression of women.” 
      
      <h3> <b> Cultural Outlooks: Women, Government, and Patriarchy </h3> </b>
      
      <p>This next graph looks at the following cultural questions from the Arab Barometer:
      
      <p>Do you agree or disagree with the following prompts:
      <ul>
      <li> A woman can become President or Prime Minister of a Muslim country.</li>
      <li> In general, men are better at political leadership than women. </li>
      <li> University education for males is more important than university education for females.</li>
      <li> It is permissible for a woman to travel abroad by herself. </li>
      <li> Women and men should have equal rights in making the decision to divorce. </li>
      <li> Husbands should have final say in all decisions concerning the family. </li>
      <li> Women’s share of inheritance should be equal to that of men. </li> 
      </ul> ")
    })
    
    output$culture_plot <- renderPlot({
        
        culture %>% 
            select(country, !!sym(input$question)) %>% 
            group_by(country, !!sym(input$question)) %>% 
            summarize(total = n()) %>% 
            mutate(pc = total/sum(total)) %>% 
            ggplot(aes(x = country, y = pc*100, fill=fct_rev(as.factor(!!sym(input$question))))) + 
            geom_bar(position="stack", stat="identity") + 
        scale_fill_manual(values=c("gray63", "indianred2", "seagreen2")) +
            labs(title = "",
                 y = "Percentage of Respondents",
                 x = "Country",
                 fill = "") +
            theme_classic() + 
            theme(axis.text.x = element_text(angle = 60, face = "bold", size = 11),
                  axis.line.x = element_blank(),
                  axis.ticks.x = element_blank())
    })
    
    output$text3 <- renderUI({
      HTML("The data reveals some important nuances regarding public opinion of 
      gender equality in the Middle East. 
      
      <p> For the question on women becoming the head of state of a Muslim country, 
      results are mixed but appear to show that countries on average
      predominantly agree that women can fulfill that role. 
      
      <p> On political leadership, however, respondents believe that men 
      are better than women. This presents an interesting 
      development of the conclusion found in the previous graph. On a policy level, 
      Middle Eastern publics believe in fairer representation for women in government. 
      However, many believe that a man is better equipped to fulfill that role, even 
      in the present of representational equality. Looking at the question on the 
      husband having the final say in family matters, the responses are quite similar.
      
      <p> This equality story persists in the question on higher education. All countries 
      in the survey report that university education is equally important for both males 
      and females. The trend is similar on decisions of divorce. Responses 
      for traveling abroad are mixed.
      
      <p> These findings point to several important conclusions. First and foremost, 
      there is no lack of public support for gender equality in norms and policy. 
      Most respondents across the countries surveyed believe that men and women alike 
      should have equal access to higher education and divorce rights. 
      However, when it came to questions that ask about beliefs, males are believed as more 
      competent and more worthy of authority. Political leadership and family authority 
      are prioritized for men regionwide. 
      
      <h3> <b> Religion and Gender Discrimination </b> </h3>
      
      <p> What lies at this disconnect between what respondents think should be permissible 
      and what they truly believe is right for society? In the above data, respondents 
      believe in certain measures that guarantee equality, but also believe that men 
      are more competent in certain roles and positions. 
      
      <p> These exclusionary and conservative outlooks, according to the Institute for 
      Policy Studies, oftentimes originate in religious teachings. However, what the 
      Institute for Policy Studies fails to demonstrate is whether support for religion 
      in public life is at odds with support for gender equality. 
      
      <p> To test this, I look at two dimensions. The first dimension (x-axis) is the support 
      for gender-discriminatory values; the higher this index, the less supportive 
      of equality one is. The second dimension (y-axis) looks at religiosity as a function 
      of support for religious authority in public life; the higher this index, the more 
      religious one is. 
")
    })
    
    output$rlg_clt_plot <- renderPlot({
        rlgclt_plot
    })
    
    output$text4 <- renderUI({
      HTML("<p>The data shows that there is a positive correlation between the two. 
      The more supportive a respondent is of gender discriminatory norms, the more 
      religious they tend to be. See the 'Models' tab for a regression model of this data.
      
      <p> <p>As such, the Middle East does in fact hold the blueprint for an effective 
      transition towards gender equality due to high public support for certain norms 
      and policy positions. However, the little progress that has been seen in gender 
      equality could be associated with religious authority. As it stands, the Middle 
      East sees religion as a major part of public life, and the final graph shows that 
      there is a positive correlation between religiosity and support for 
      gender-discriminatory norms.")
    })
    
    output$text5 <- renderUI({
      HTML("<h2>Model Explanations</h2>
      
      <h3> <b> Support for Women in the Government </b> </h3>
      
      <p> For the first model, I create a simple graph to find general 
           attitudes regarding women in the government. I extract data from the
           Arab Barometer (2018-2019) and select for Question 601a: 
           'Some people think in order to achieve fairer representation a 
           certain percentage of elected positions should be set aside for women.
           To what extent do you agree with this statement?' 1 is strongly agree, 
           4 is strongly disagree. 
           
          <p> The reason I choose this question is because it provides a
           general overview of the position held by the Middle East public
           regarding a policy decision geared towards equality. It sets up the
           remainder of this project to look at cultural outlooks and the relationship
           with religion. 
           
        <h3> <b>   Cultural Outlooks: Women, Government, and Patriarchy </h3> </b>
           
           <p> For the second model, I complete several important transformations in order
           to achieve a digestible visualization. First and foremost, I select 
           for the questions of interest in the Arab Barometer, which all fall 
           under Q601. I transform responses of “1. Strongly agree” and “2. Agree” 
           to just “Agree,” while doing the same for “Disagree” (values 3 and 4). 
           As such, I can visualize the data in a way that makes sense rather than 
           having 4 response values that might confuse. 
           
           <p> Then, I change the names for the country codes so that my Shiny app 
           does not have to complete a lot of work in order to change the names 
           every time an input is selected. I use the replace function in R. 
           
           <p> I choose a stacked bar chart in order to visualize how these responses 
           change depending on the prompt across a wide array of countries. 
           Turning these values into percentages rather than counts felt like 
           an appropriate way of showing the true proportions within each country. 
           
           <h3> <b> Religion and Gender Discrimination </h3> <b>
           ")
    })
    
    output$rlgclt_plot <- renderPlot({
      rlgclt_plot
    })
    
    output$text6 <- renderUI({
      HTML("For my final model, I want to find the correlation, if any, between 
      the average score per respondent on matters relating to gender and their 
      religiosity.
      
      <p>I transform all of the responses so that 1 corresponds with a pro-gender 
      equality stance , and 4 corresponds with an anti-gender equality stance. 
      By averaging out the responses for each respondent along the questions from 
      the previous plot, I created an “equality index,” or an average attitude 
      toward anti-discriminatory norms (where 1 is pro-equality and 4 is anti-equality). 
      
      <p>I do the same thing for my religiosity index. I use the 5 sub-questions from Q606: 
      <ul>
      <li> Religious leaders should not interfere in voters’ decisions in elections.</li>
      <li> Your country is better off if religious people hold public positions in the state.</li>
      <li> Religious clerics should have influence over the decisions of government.</li>
      <li> Religious practice is a private matter and should be separated from socio-economic life.</li>
      <li> Today, religious leaders are as likely to be corrupt as non-religious leaders.</li>
      </ul>
      
      <p>I transform the responses to these variables so that 1 corresponds with being against religion in public life (and therefore a proxy for being non-religious), while 4 correspond with being for religion in public life (and therefore a proxy for being religious). By averaging out the responses for each respondent along the questions from the previous plot, I created an “religiosity index.”
      
      <p>Below is the correlation coefficient between the two indices: 
")
    })
    
    output$rlgclt_cor <- renderPrint({
      rlg_clt_cor
    })
    
    output$text7 <- renderUI({
      HTML("Below is the regression table for this analysis. It shows that 
           increasing the religiosity_index by 1 will translate into a 0.28 
           increase in support for gender discriminatory values.")
    })
    
    output$rlgclt_reg <- renderPrint({
      rlg_clt_reg
    })
    
    output$about <- renderUI({
      HTML("<h3> <b> Project Background and Motivation </h3> </b>
      
      <p> This project is a data-driven exploration of a controversial topic: the 
      role and rights of women in the Middle East. It is often the stereotype that 
      women are underrepresented and marginalized in Arab communities due to 
      patriarchal norms and values. This project assesses whether these stereotypes 
      hold any ground through an analysis of public opinion. At the same time, it 
      seeks to distinguish between values that relate to policy and values that 
      relate to cultural norms. It finds that, on average, support for gender 
      inclusive and non-discriminatory policy is widespread, while support for 
      non-discriminatory cultural norms is less obvious. The project then problematizes 
      the view that religion often lies at the core of these gender issues 
      by creating an equality index and a religiosity index.
      
      <h3> <b> The Arab Barometer </h3> </b>
      
      <p>Data for this project was accessed through the Arab Barometer, 
      “a nonpartisan research network that provides insight into the social, 
      political, and economic attitudes and values of ordinary citizens 
      across the Arab world.”
      
      <p>This project relies on the fifth wave of the Arab Barometer, which was 
      conducted between the years of 2018 and 2019. The data can be accessed 
      <a href=https://www.arabbarometer.org/waves/arab-barometer-wave-v>here</a>.
      
      <h3> <b> About Me </h3> </b>
      
      <p> My name is Jad Maayah and I am a junior data scientist and senior at 
      Harvard College completing his studies in Government and Economics. 
      I was born and raised in Amman, Jordan, which has fostered a far-reaching 
      interest in Middle Eastern politics and law as well as gender issues in my 
      kingdom and the countries that surround it. I hope to continue exploring my 
      interests at law school in order to better understand the legal contexts around 
      immigration and civil rights. 
      
      <p> I can be reached at jjmaayah@gmail.com.
      
      <p>My GitHub repository for this project can be accessed 
      <a href=https://github.com/jadmaayah/women-religion-mena>here</a>. 

")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
