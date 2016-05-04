from django.shortcuts import render, get_object_or_404
from django.template import loader

from django.http import HttpResponse, Http404

from .models import *


from jinja2 import Environment
from django.contrib.staticfiles.storage import staticfiles_storage
from django.core.urlresolvers import reverse


def environment(**options):
    env = Environment(**options)
    env.globals.update({
       'static': staticfiles_storage.url,
       'url': reverse,
    })
    return env


# Create your views here.
def index(request):
    red_players = Player.objects.filter(team_id=1)
    blue_players = Player.objects.filter(team_id=2)

    template = loader.get_template('players/index.html')
    context = {
        'red_players': red_players,
        'blue_players': blue_players,
    }
    return render(request, 'players/index.html', context)


def player_page(request, player_id):
    player = get_object_or_404(Player, pk=player_id)

    context = {'player': player,
               'red_team': Team.objects.get(pk=1),
               'blue_team': Team.objects.get(pk=2),
               }
    return render(request, 'players/player_page.html', context)


def detail(request, question_id):
    question = get_object_or_404(Question, pk=question_id)
    return render(request, 'players/detail.html', {'question': question})


def results(request, question_id):
    response = "You're looking at the results of question %s"
    return HttpResponse(response % question_id)


def vote(request, question_id):
    return HttpResponse("You're voting on question %s." % question_id)