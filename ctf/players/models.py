import datetime

from django.db import models
from django.utils import timezone


# Create your models here.
class Team(models.Model):
    RED_TEAM = 'r'
    BLUE_TEAM = 'b'
    TEAM_CHOICES = ((RED_TEAM, 'Red'),
                    (BLUE_TEAM, 'Blue'),
                    )

    team_name = models.CharField(max_length=1,
                                 choices=TEAM_CHOICES,
                                 default=RED_TEAM)
    team_colour = models.CharField(max_length=6, name="Hexcode team colour")

    score = models.IntegerField(default=0)


class Player(models.Model):
    LIGHT_CLASS = 'li'
    MEDIUM_CLASS = 'me'
    HEAVY_CLASS = 'he'
    CLASS_CHOICES = ((LIGHT_CLASS, 'Light Class'),
                     (MEDIUM_CLASS, 'Medium Class'),
                     (HEAVY_CLASS, 'Heavy CLass'),
                     )

    # Character info
    team = models.ForeignKey(Team, on_delete=models.CASCADE)
    player_name = models.CharField(max_length=20)

    # play information
    kills = models.IntegerField(default=0)
    deaths = models.IntegerField(default=0)

    player_class = models.CharField(max_length=2,
                                    choices=CLASS_CHOICES,
                                    default=MEDIUM_CLASS)


class Question(models.Model):
    question_text = models.CharField(max_length=200)
    pub_date = models.DateTimeField('date published')

    def was_published_recently(self):
        return self.pub_date >= timezone.now() - datetime.timedelta(days=1)

    def __str__(self):
        return self.question_text


class Choice(models.Model):
    question = models.ForeignKey(Question, on_delete=models.CASCADE)
    choice_text = models.CharField(max_length=200)
    votes = models.IntegerField(default=0)

    def __str__(self):
        return self.choice_text