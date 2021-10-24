{ lib, baseEmail }:
let
  rawFeeds = {
    "blogs" = {
      "Garry's Blog" =
        "http://feeds.feedburner.com/garrys_blog";
      "Hyperbole and a Half" =
        "http://feeds.feedburner.com/Hyperbole-and-a-half";
      "Mastering Emacs" =
        "http://www.masteringemacs.org/feed/";
    };

    "comic" = {
      "Safely Endangered" =
        "http://www.webtoons.com/en/comedy/safely-endangered/rss?title_no=352";
      "The Perry Bible Fellowship" =
        "http://pbfcomics.com/feed/feed.xml";
      "The Oatmeal" =
        "http://feeds.feedburner.com/oatmealfeed";
      "xkcd" =
        "http://www.xkcd.com/rss.xml";
      "Buttersafe" =
        "http://feeds.feedburner.com/Buttersafe";
      "Shen Comix" =
        "http://shencomix.tumblr.com/rss";
      "Laws and Sausages" =
        "http://lawsandsausagescomic.com/comic/rss";
      "El Goonish Shive" =
        "https://egscomics.com/comic/rss";
      "EGS:NP" =
        "http://www.egscomics.com/egsnp/rss";
      "EGS Sketchbook" =
        "http://www.egscomics.com/sketchbook/rss";
      "I want to be a cute anime girl" =
        "https://www.webtoons.com/en/challenge/i-want-to-be-a-cute-anime-girl/rss?title_no=349416";
      "A Lesson Is Learned But The Damage Is Irreversible" =
        "http://www.kafsemo.org/rss/filters/live/http___www_alessonislearned_com_archive_php.rss";
      "Daily Life of Dia" =
        "https://www.webtoons.com/en/challenge/daily-life-of-dia/rss?title_no=94272";
      "SMBC" =
        "http://www.smbc-comics.com/rss.php";
      "Goodbye to Halos" =
        "http://www.goodbyetohalos.com/feed/";
      "The Nerds of Paradise" =
        "http://www.thenerdsofparadise.com/nerdsfeed.xml";
      "Fangs Feed" =
        "https://fangs.ink/feed.xml";
      "Bodies" =
        "https://www.webtoons.com/en/challenge/bodies/rss?title_no=313877";
      "Order of the Stick" =
        "http://www.giantitp.com/comics/oots.rss";
    };

    "comic-backlog" = {
      "Castle Swimmer" =
        "https://www.webtoons.com/en/fantasy/castle-swimmer/rss?title_no=1499";
      "The Glass Scientists" =
        "http://www.theglassscientists.com/?feed=rss2";
      "Rain" =
        "http://rain.thecomicseries.com/rss/";
      "Paranatural" =
        "http://www.paranatural.net/rss.php";
      "Dead Winter" =
        "http://deadwinter.cc/dwrss.xml";
    };

    "games" = {
      "Introversion Blog" =
        "http://forums.introversion.co.uk/defcon/introversion/rss.php?blog=true";
      "Tomorrow Corporation" =
        "http://tomorrowcorporation.com/feed/";
      "Introversion" =
        "http://forums.introversion.co.uk/introversion/rss.php";
      "GOG Connect RSS Feed" =
        "https://steamcommunity.com/groups/gogcomco/rss/";
    };

    "other" = {
      "NixOS News" =
        "https://nixos.org/news-rss.xml";
      "Planet Lisp" =
        "http://planet.lisp.org/rss20.xml";
      "The Codeless Code" =
        "http://thecodelesscode.com/rss";
    };

    "other-backlog" = {
      "Deeplinks" =
        "http://www.eff.org/rss/updates.xml";
      "APOD" =
        "http://apod.nasa.gov/apod.rss";
    };
  };
  emailParts = builtins.elemAt (builtins.split "([^@]+)@(.+)" baseEmail) 1;
  emailPrefix = builtins.elemAt emailParts 0;
  emailDomain = builtins.elemAt emailParts 1;
  enrichEmail = category: "${emailPrefix}+${category}@${emailDomain}";
  mkFeedPair = category: title: url: lib.nameValuePair title {
    inherit url;
    toEmail = enrichEmail category;
  };
  mkFeedPairs = category: feeds: lib.mapAttrsToList (mkFeedPair category) feeds;
  feedPairs = lib.concatLists (lib.mapAttrsToList mkFeedPairs rawFeeds);
  feeds = builtins.listToAttrs feedPairs;
in feeds
