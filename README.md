# leak-of-legends
A project for the Applied Statistics university course at Sofia University: Analysis of data from League of Legends games.

## Is it any good?
Most probably, no.

## Statistical analysis



### Short description of League of Legends

There are two teams: blue and red, which have their bases at the bottom left and upper right corners accordingly. Each team consists of 5 player, playing 5 unique (in the team) champions and strives to destroy the base of the enemy. Before being able to go to the enemy base the team has to destroy the defense towers placed on the enemy side of the terrain. Periodically, the base of each team summons a wave of minions, which move toward the enemy base on each of the three lanes and mindlessly attack every enemy unit on their way.

![map of the game](images/map.jpg)



There's an unwritten rule or tactic, which virtually all teams play by: there is an Attack Damage carry (ranged fighter, which inflicts physical damage) and a healer on the bottom lane, a mage on the middle lane, one champion on the top lane and one inside the jungle (who every now and then will attempt surprise attacks on the lanes).



### Problem #1: is it possible to predict (with at least some degree of accuracy) the outcome of the game only by the champions selected for each role in the two teams.

The expected result is that we shouldn't be able to do such prediction (at least not with high degree of accuracy), because otherwise it will mean that the game is really imbalanced and favors some champions over others. On the other hand, it is true that some champions are good counters of others and hence, it may turn out, that it is possible to devise a model which is little bit better than the random model (which obviously should has 50% success rate).



#### Creating a test set

~~It would be desirable to have a lot of game records for each possible combination of champions. However, for the period our dataset is from (games between 2015 and 2017), the number of champions in the game was between 125 and 135. This means that the number of possible combinations is around  $$ 2 \times \binom{135}{5} = 693400554$$, because the same champion can be picked in both teams). Our dataset contains the data for only 3645, so clearly we don't have such amounts of  data.~~



We take only the picked champions and the result of the game.

```R
picks_and_result <- LeagueofLegends[, c(6, 11, 13, 15, 17, 19, 21, 23, 25, 27, 28)]
```



```R
> picks_and_result
# A tibble: 3,645 × 11
   bResult blueTopChamp blueJungleChamp blueMiddleChamp blueADCChamp blueSupportChamp redTopChamp redJungleChamp redMiddleChamp redADCChamp redSupportChamp
     <int>        <chr>           <chr>           <chr>        <chr>            <chr>       <chr>          <chr>          <chr>       <chr>           <chr>
1        1       Irelia          RekSai            Ahri         Jinx            Janna        Gnar          Elise           Fizz       Sivir          Thresh
2        0         Gnar          Rengar            Ahri      Caitlyn            Leona      Irelia       JarvanIV           Azir       Corki           Annie
3        1     Renekton          Rengar            Fizz        Sivir            Annie        Sion         LeeSin           Azir       Corki           Janna
4        0       Irelia        JarvanIV         Leblanc        Sivir           Thresh        Gnar           Nunu           Lulu      KogMaw           Janna
5        1         Gnar        JarvanIV       Lissandra     Tristana            Janna        Sion         RekSai           Lulu       Corki           Annie
6        0     Kassadin          Rengar         Leblanc        Sivir            Annie        Gnar       JarvanIV           Lulu       Corki          Thresh
7        1       Irelia        JarvanIV          Xerath        Corki            Janna    Renekton         LeeSin        Leblanc    Tristana            Nami
8        1     Renekton        JarvanIV            Azir      Caitlyn            Annie      Rumble         Rengar        Leblanc       Sivir      Blitzcrank
9        0         Sion          RekSai         Orianna       KogMaw            Janna    Kassadin             Vi            Zed       Corki         Morgana
10       1       Irelia        Nocturne         Orianna        Sivir             Nami        Gnar         Rengar        Leblanc      Graves         Morgana
# ... with 3,635 more rows
```



We seed the random generator:

```R
set.seed(42)
```



We pick random indexes for creating a training set:

```R
training_set_indexes = sample(nrow(picks_and_result), 1500)
```



We then take the training subset from the whole dataset:

```R
training_set <- picks_and_result[training_set_indexes, ]
```



And create a test set with all rows which aren't part of the training set:

```R
test_set <- picks_and_result[-training_set_indexes, ]
```



Convert all predictor variables to factors:

```R
training_set$blueTopChamp <- as.factor(training_set$blueTopChamp)
training_set$blueJungleChamp <- as.factor(training_set$blueJungleChamp)
training_set$blueMiddleChamp <- as.factor(training_set$blueMiddleChamp)
training_set$blueADCChamp <- as.factor(training_set$blueADCChamp)
training_set$blueSupportChamp <- as.factor(training_set$blueSupportChamp)

training_set$redTopChamp <- as.factor(training_set$redTopChamp)
training_set$redJungleChamp <- as.factor(training_set$redJungleChamp)
training_set$redMiddleChamp <- as.factor(training_set$redMiddleChamp)
training_set$redADCChamp <- as.factor(training_set$redADCChamp)
training_set$redSupportChamp <- as.factor(training_set$redSupportChamp)
```


We train a decision tree:

```R
model <- rpart(bResult ~ ., data=training_set, method="class")
plot(model)
text(model)
```

(We have printed it without pretty print, since otherwise we get a list of many champion names for each branch and it's very cluttered and unreadable)

![decision tree](ugly_print_decision_tree_for_champ_picks.png)



When we try to do a prediction on the test set, it turns out that there's a problem: 

```R
predict(model, test_set, type="class")
Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = attr(object,  : 
  factor blueTopChamp has new levels Akali, Alistar, Diana, Kayle, Khazix, LeeSin, Lucian, Nocturne, Nunu, Rengar, Varus, Yorick, Zac
```



Since our dataset is really not big enough, it is normal that some of the Role/Champion pairs that are present in the test set didn't appear in the training set. Lets see how dire is the situation:

```R
length(test_set$blueTopChamp[!(test_set$blueTopChamp %in% training_set$blueTopChamp)])
[1] 22
```



The above piece of code tells us how many rows in the test set have champions for the "Top lane" role that were never played for that role in the training set. These rows cannot be predicted and we should remove them.  We want to see if there's too many such rows. The total number of such rows will be less or equal than the sum of all unknown role/champion pairs for each predictor. Lets check this out:

```R
count <- 0
for (predictor in names(test_set)[2:11]) {
    count <- count + length(get(predictor, test_set)[!(get(predictor, test_set) %in% get(predictor, training_set))])
}
count
[1] 105
```



Less than 105 out of 2145, this seems okay. Lets remove these unpredictable cases from the test set. (Note that the more data we have the more this problem will disappear. Also, we can employ a more advanced algorithm for creating a training set, but let's keep it simple)