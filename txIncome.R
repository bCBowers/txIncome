library(stringr)
library(ggplot2)
library(plyr)

opts_string = {theme(axis.text.x=element_text(size=rel(2),angle=0),
                     axis.text.y=element_text(size=rel(2)),
                     legend.text=element_text(size=rel(2)),
                     title=element_text(size=rel(2)),
                     panel.background=element_blank(),
                     panel.border=element_rect(color='black',fill=NA),
                     panel.grid.major=element_line(colour='grey20',linetype='dotted'),
                     panel.grid.minor=element_line(color='grey',linetype='dotted'))}

raw_data <- read.csv('CCES12_Common_VV.csv')
texas <- raw_data[raw_data$StateAbbr == 'TX', ]
texas_red <- texas[c('V101', 'weight_vv', 'faminc', 'CC304', 'CC354', 'CC355', 'CC355b', 'CC410b', 'CC417bx_2', 'ideo5')]

######## Recode relevant questions to get readable responses

# Recode Family Income
texas_red$faminc[texas_red$faminc ==1]= 'Less than $10,000'
texas_red$faminc[texas_red$faminc ==2]= '$10,000 - $19,999'
texas_red$faminc[texas_red$faminc ==3]= '$20,000 - $29,999'
texas_red$faminc[texas_red$faminc ==4]= '$30,000 - $39,999'
texas_red$faminc[texas_red$faminc ==5]= '$40,000 - $49,999'
texas_red$faminc[texas_red$faminc ==6]= '$50,000 - $59,999'
texas_red$faminc[texas_red$faminc ==7]= '$60,000 - $69,999'
texas_red$faminc[texas_red$faminc ==8]= '$70,000 - $79,999'
texas_red$faminc[texas_red$faminc ==9]= '$80,000 - $99,999'
texas_red$faminc[texas_red$faminc ==10]= '$100,000 - $119,999'
texas_red$faminc[texas_red$faminc ==11]= '$120,000 - $149,999'
texas_red$faminc[texas_red$faminc ==12]= '$150,000 - $199,999'
texas_red$faminc[texas_red$faminc ==13]= '$200,000 - $249,999'
texas_red$faminc[texas_red$faminc ==14]= '$250,000 - $349,999'
texas_red$faminc[texas_red$faminc ==15]= '$350,000 - $499,999'
texas_red$faminc[texas_red$faminc ==16]= '$500,000 or more'
texas_red$faminc[texas_red$faminc ==31]= '$150,000 or more'
texas_red$faminc[texas_red$faminc ==18]= '$250,000 or more'
texas_red$faminc[texas_red$faminc ==97]= 'Prefer not to say'
texas_red$faminc[texas_red$faminc ==98]= 'Skipped'
texas_red$faminc[texas_red$faminc ==99]= 'Not asked'

# Record CC304 about increasing income -> Not included in blog post
texas_red$CC304[texas_red$CC304 ==1]= 'Increased a lot'
texas_red$CC304[texas_red$CC304 ==2]= 'Increased somewhat'
texas_red$CC304[texas_red$CC304 ==3]= 'Stayed about the same'
texas_red$CC304[texas_red$CC304 ==4]= 'Decreased somewhat'
texas_red$CC304[texas_red$CC304 ==5]= 'Decreased a lot'
texas_red$CC304[texas_red$CC304 ==8]= 'Skipped'
texas_red$CC304[texas_red$CC304 ==9]= 'Not asked'

# Combine voting preference questions and recode -> not used since question is pre-election
texas_red$senVote = texas_red$CC355
texas_red$senVote[is.na(texas_red$senVote)==TRUE]=texas_red$CC355b[is.na(texas_red$senVote)==TRUE]

texas_red$senVote[texas_red$senVote ==1] = 'Paul Sadler'
texas_red$senVote[texas_red$senVote ==2] = 'Ted Cruz'
texas_red$senVote[texas_red$senVote ==3] = '3rd Party'
texas_red$senVote[texas_red$senVote ==7] = 'Other'
texas_red$senVote[texas_red$senVote ==8] = 'Im not sure'
texas_red$senVote[texas_red$senVote ==9] = 'No one'
texas_red$senVote[texas_red$senVote ==98] = 'Skipped'
texas_red$senVote[texas_red$senVote ==99] = 'Not asked'

# Recode did you vote -> not used since CC410b recorded actual voter behavior
texas_red$CC354[texas_red$CC354 == 1] = 'Likely voter'
texas_red$CC354[texas_red$CC354 == 2] = 'Likely voter'
texas_red$CC354[texas_red$CC354 == 3] = 'Likely voter'
texas_red$CC354[texas_red$CC354 == 4] = 'Likely voter'
texas_red$CC354[texas_red$CC354 == 5] = 'No'
texas_red$CC354[texas_red$CC354 == 6] = 'Undecided'
texas_red$CC354[texas_red$CC354 == 8] = 'Skipped'
texas_red$CC354[texas_red$CC354 == 9] = 'Not asked'

# Recode ideology
texas_red$ideo5[texas_red$ideo5 == 1] = 'Very Liberal'
texas_red$ideo5[texas_red$ideo5 == 2] = 'Liberal'
texas_red$ideo5[texas_red$ideo5 == 3] = 'Moderate'
texas_red$ideo5[texas_red$ideo5 == 4] = 'Conservative'
texas_red$ideo5[texas_red$ideo5 == 5] = 'Very Conservative'
texas_red$ideo5[texas_red$ideo5 == 6] = 'Not Sure'
texas_red$ideo5[texas_red$ideo5 == 8] = 'Skipped'
texas_red$ideo5[texas_red$ideo5 == 9] = 'Not Asked'

# Recode actual voting totals
texas_red$CC410b[texas_red$CC410b ==1] = 'Paul Sadler'
texas_red$CC410b[texas_red$CC410b ==2] = 'Ted Cruz'
texas_red$CC410b[texas_red$CC410b ==3] = '3rd Party'
texas_red$CC410b[texas_red$CC410b ==7] = 'Other'
texas_red$CC410b[texas_red$CC410b ==8] = 'Im not sure'
texas_red$CC410b[texas_red$CC410b ==9] = 'No one'
texas_red$CC410b[texas_red$CC410b ==98] = 'Skipped'
texas_red$CC410b[texas_red$CC410b ==99] = 'Not asked'


########## Income Level by Candidate

# Clean up family income (32 is a typo)
texas_red <- texas_red[texas_red$faminc!='32', ]
texas_red <- texas_red[texas_red$faminc!='Prefer not to say', ]

# Plot Income by Candidate
twoCand <- texas_red[texas_red$CC410b=='Paul Sadler' | texas_red$CC410b=='Ted Cruz', ]
twoCand <-twoCand[!is.na(twoCand$CC410b), ]

plotData <- aggregate(twoCand$faminc, by=list(Candidate=twoCand$CC410b, Income=twoCand$faminc), FUN=length)
plotData$Income <- factor(plotData$Income,levels = c('Less than $10,000', '$10,000 - $19,999', '$20,000 - $29,999', '$30,000 - $39,999',
                                                     '$40,000 - $49,999', '$50,000 - $59,999', '$60,000 - $69,999', '$70,000 - $79,999', 
                                                     '$80,000 - $99,999', '$100,000 - $119,999', '$120,000 - $149,999', '$150,000 - $199,999',
                                                     '$200,000 - $249,999', '$250,000 - $349,999', '$350,000 - $499,999', '$500,000 or more'))

ggplot(plotData, aes(x=Income, y=x, fill = Candidate)) + 
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() + xlab('Income Level\n') + coord_flip() + scale_fill_manual(values=c('blue','red') ) +
  ylab('Voters\n') +ggtitle('Income Levels for 2012 Texas Senate \nVoters by Candidate') +
  scale_y_continuous() + opts_string
ggsave('tedChart.png',units=c('cm'),width=50,height=50)

# Identify median voter
paulVote <- twoCand[twoCand$CC410=='Paul Sadler', ]
paulVote <-paulVote[!is.na(paulVote$CC410b), ]
nrow(paulVote)
aggregate(paulVote$CC410b, by=list(Income = paulVote$faminc), FUN=length)

tedVote <- twoCand[twoCand$CC410b=='Ted Cruz', ]
tedVote <-tedVote[!is.na(tedVote$CC410b), ]
nrow(tedVote)
aggregate(tedVote$CC410b, by=list(Income = tedVote$faminc), FUN=length)

########### Income level by Ideology

plotData <- aggregate(texas_red$faminc, by=list(Ideology=texas_red$ideo5, Income=texas_red$faminc), FUN=length)

plotData$Income <- factor(plotData$Income,levels = c('Less than $10,000', '$10,000 - $19,999', '$20,000 - $29,999', '$30,000 - $39,999',
                                                     '$40,000 - $49,999', '$50,000 - $59,999', '$60,000 - $69,999', '$70,000 - $79,999', 
                                                     '$80,000 - $99,999', '$100,000 - $119,999', '$120,000 - $149,999', '$150,000 - $199,999',
                                                     '$200,000 - $249,999', '$250,000 - $349,999', '$350,000 - $499,999', '$500,000 or more'))

plotData <- plotData[plotData$Ideology!='Not Sure', ]

plotData$Ideology <- factor(plotData$Ideology, levels=c('Very Conservative', 'Conservative', 'Moderate', 'Liberal', 'Very Liberal'))

ggplot(plotData, aes(x=Income, y=x, fill = Ideology)) + 
  geom_bar(stat="identity", color="black")+ scale_fill_manual(values=c('dark red','red', 'purple', 'blue', 'dark blue') ) +
  theme_minimal() + xlab('Income Level\n') + coord_flip() + 
  ylab('Voters\n') +ggtitle('Income Levels for 2012 Texas Senate \nVoters by Ideology') +
  scale_y_continuous() + opts_string
ggsave('tedIdeo.png',units=c('cm'),width=50,height=50)



######### Candidate by Income Delta -> shows Republicans hating economy and Democrats loving it (causation is probably backwards though)

twoCand <- texas_red[texas_red$CC410b=='Paul Sadler' | texas_red$CC410b=='Ted Cruz', ]
twoCand <-twoCand[!is.na(twoCand$CC410b), ]

plotData <- aggregate(twoCand$CC304, by=list(Candidate=twoCand$CC410b, Income=twoCand$CC304), FUN=length)

plotData$Income <- factor(plotData$Income, levels=c('Decreased a lot', 'Decreased somewhat', 'Stayed about the same', 'Increased somewhat', 'Increased a lot'))


ggplot(plotData, aes(x=Income, y=x, fill = Candidate)) + 
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() + xlab('Income Change\n') + coord_flip() + scale_fill_manual(values=c('blue','red') ) +
  ylab('Voters\n') +ggtitle('Income Changefor 2012 Texas Senate \nVoters by Candidate') +
  scale_y_continuous() + opts_string
ggsave('incomeDelta.png',units=c('cm'),width=50,height=50)

