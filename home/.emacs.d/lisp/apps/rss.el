;;; apps/rss.el --- extracted config -*- lexical-binding: t; -*-

;;; Code:


(use-package elfeed
  :ensure t
  :defer t
  :config

  (setq elfeed-feeds
        '("http://lambda-the-ultimate.org/rss.xml"
          "http://planet.emacsen.org/atom.xml"
          "http://www.overcomingbias.com/feed"
          "http://slatestarcodex.com/feed/"
          "http://worrydream.com/feed.xml"
          "https://xkcd.com/rss.xml"
          "http://existentialcomics.com/rss.xml"
          "http://joshldavis.com/atom.xml"
          "https://rationalconspiracy.com/feed/"
          "https://soylentnews.org/index.rss"
          "http://meaningness.com/rss.xml"
          "http://feeds.ribbonfarm.com/Ribbonfarm"
          "http://www.cs.uni.edu/~wallingf/blog/index.xml"
          ;;"https://feeds.feedburner.com/Metafilter"
          "http://feeds.feedburner.com/thoughtsfromtheredplanet?format=xml"
          "http://www.gwern.net/atom.xml"
          "http://airspeedvelocity.net/feed/")))


(setq url-queue-timeout 30)

(provide 'apps/rss)
;;; apps/rss.el ends here
