//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

import fluxer_admin/components/review_keyboard
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

pub fn styles() -> element.Element(a) {
  let css =
    "
  [data-review-deck] { position: relative; }
  [data-review-card] { will-change: transform, opacity; touch-action: pan-y; }
  [data-review-card][hidden] { display: none !important; }

  .review-card-enter { animation: reviewEnter 120ms ease-out; }
  @keyframes reviewEnter { from { opacity: .6; transform: translateY(6px) scale(.995);} to { opacity: 1; transform: translateY(0) scale(1);} }

  .review-card-leave-left { animation: reviewLeaveLeft 180ms ease-in forwards; }
  .review-card-leave-right { animation: reviewLeaveRight 180ms ease-in forwards; }
  @keyframes reviewLeaveLeft { to { opacity: 0; transform: translateX(-120%) rotate(-10deg);} }
  @keyframes reviewLeaveRight { to { opacity: 0; transform: translateX(120%) rotate(10deg);} }

  .review-toast { position: fixed; left: 16px; right: 16px; bottom: 16px; z-index: 80; }
  .review-toast-inner { max-width: 720px; margin: 0 auto; }

  .review-hintbar { position: sticky; bottom: 0; z-index: 10; }
  .review-kbd { font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace; }
  "
  h.style([a.type_("text/css")], css)
}

pub fn script_tags() -> List(element.Element(a)) {
  [
    review_keyboard.script_tag(),
    h.script([a.attribute("defer", "defer")], script()),
  ]
}

pub fn script() -> String {
  "
(function () {
  function qs(el, sel) { return el.querySelector(sel); }
  function qsa(el, sel) { return Array.prototype.slice.call(el.querySelectorAll(sel)); }

  function clamp(n, min, max) { return Math.max(min, Math.min(max, n)); }

  function showToast(message) {
    var toast = document.createElement('div');
    toast.className = 'review-toast';
    toast.innerHTML =
      '<div class=\"review-toast-inner\">' +
      '<div class=\"bg-red-50 border border-red-200 text-red-800 rounded-xl px-4 py-3 shadow-lg\">' +
      '<div class=\"text-sm font-semibold\">Action failed</div>' +
      '<div class=\"text-sm mt-1\" style=\"word-break: break-word;\">' + (message || 'Unknown error') + '</div>' +
      '</div>' +
      '</div>';
    document.body.appendChild(toast);
    setTimeout(function () { toast.remove(); }, 4200);
  }

  function parseHTML(html) {
    var parser = new DOMParser();
    return parser.parseFromString(html, 'text/html');
  }

  function asURL(url) {
    try { return new URL(url, window.location.origin); } catch (_) { return null; }
  }

  function enhanceDeck(deck) {
    var cards = qsa(deck, '[data-review-card]');
    var idx = 0;

    var fragmentBase = deck.getAttribute('data-fragment-base') || '';
    var nextPage = parseInt(deck.getAttribute('data-next-page') || '0', 10);
    var canPaginate = deck.getAttribute('data-can-paginate') === 'true';
    var prefetchWhenRemaining = parseInt(deck.getAttribute('data-prefetch-when-remaining') || '6', 10);
    var prefetchInFlight = false;

    var emptyUrl = deck.getAttribute('data-empty-url') || '';

    function currentCard() { return cards[idx] || null; }
    function remainingCount() { return Math.max(0, cards.length - idx); }

    function setHiddenAllExcept(active) {
      for (var i = 0; i < cards.length; i++) {
        var c = cards[i];
        c.hidden = (c !== active);
      }
    }

    function updateUrlFor(card) {
      var directUrl = card && card.getAttribute('data-direct-url');
      if (!directUrl) return;
      try {
        history.replaceState({ review: true, directUrl: directUrl }, '', directUrl);
      } catch (_) {}
    }

    function focusCard(card) {
      if (!card) return;
      requestAnimationFrame(function () {
        try { card.focus({ preventScroll: true }); } catch (_) { try { card.focus(); } catch (_) {} }
      });
    }

    function ensureActiveCard() {
      var card = currentCard();
      if (!card) {
        if (emptyUrl) {
          try { history.replaceState({}, '', emptyUrl); } catch (_) {}
        }
        deck.dispatchEvent(new CustomEvent('review:empty'));
        return;
      }
      setHiddenAllExcept(card);
      card.classList.remove('review-card-leave-left', 'review-card-leave-right');
      card.classList.add('review-card-enter');
      setTimeout(function () { card.classList.remove('review-card-enter'); }, 160);
      updateUrlFor(card);
      focusCard(card);
      maybePrefetchDetails(card);
      maybePrefetchMore();
      updateProgress();
    }

    function updateProgress() {
      var el = qs(deck, '[data-review-progress]');
      if (!el) return;
      el.textContent = remainingCount().toString() + ' remaining';
    }

    async function backgroundSubmit(form) {
      var actionUrl = asURL(form.action);
      if (!actionUrl) throw new Error('Invalid action URL');
      actionUrl.searchParams.set('background', '1');

      var fd = new FormData(form);
      var body = new URLSearchParams();
      fd.forEach(function (v, k) { body.append(k, v); });

      var resp = await fetch(actionUrl.toString(), {
        method: 'POST',
        headers: { 'content-type': 'application/x-www-form-urlencoded;charset=UTF-8' },
        body: body.toString(),
        credentials: 'same-origin'
      });

      if (resp.status === 204) return;
      var text = '';
      try { text = await resp.text(); } catch (_) {}
      if (!resp.ok) throw new Error(text || ('HTTP ' + resp.status));
    }

    function advance() {
      idx = idx + 1;
      ensureActiveCard();
    }

    function animateAndAdvance(card, dir) {
      card.classList.remove('review-card-enter');
      card.classList.add(dir === 'left' ? 'review-card-leave-left' : 'review-card-leave-right');
      setTimeout(function () { advance(); }, 190);
    }

    async function act(dir) {
      var card = currentCard();
      if (!card) return;

      if (dir === 'left' && card.getAttribute('data-left-mode') === 'skip') {
        animateAndAdvance(card, 'left');
        return;
      }

      var form = qs(card, 'form[data-review-submit=\"' + dir + '\"]');
      if (!form) {
        animateAndAdvance(card, dir);
        return;
      }

      try {
        await backgroundSubmit(form);
        animateAndAdvance(card, dir);
      } catch (err) {
        showToast((err && err.message) ? err.message : String(err));
      }
    }

    function onKeyDown(e) {
      var t = e.target;
      if (t && (t.tagName === 'INPUT' || t.tagName === 'TEXTAREA' || t.isContentEditable)) return;

      if (e.key === 'Escape' && emptyUrl) {
        try { history.replaceState({}, '', emptyUrl); } catch (_) {}
      }
    }

    function onKeyboardAction(e) {
      var dir = e.detail && e.detail.direction;
      if (dir === 'left' || dir === 'right') {
        act(dir);
      }
    }

    function wireButtons(card) {
      var leftBtn = qs(card, '[data-review-action=\"left\"]');
      var rightBtn = qs(card, '[data-review-action=\"right\"]');

      if (leftBtn) {
        leftBtn.addEventListener('click', function (e) {
          e.preventDefault();
          if (window.fluxerReviewKeyboard) {
            window.fluxerReviewKeyboard.enable(deck);
          }
          act('left');
        }, { capture: true });
      }

      if (rightBtn) {
        rightBtn.addEventListener('click', function (e) {
          e.preventDefault();
          if (window.fluxerReviewKeyboard) {
            window.fluxerReviewKeyboard.enable(deck);
          }
          act('right');
        }, { capture: true });
      }

      var forms = qsa(card, 'form[data-review-submit]');
      forms.forEach(function (f) {
        f.addEventListener('submit', function (e) {
          e.preventDefault();
          var dir = f.getAttribute('data-review-submit');
          if (dir === 'left' || dir === 'right') {
            if (window.fluxerReviewKeyboard) {
              window.fluxerReviewKeyboard.enable(deck);
            }
          }
          act(dir);
        }, { capture: true });
      });
    }

    function wireAll() { cards.forEach(wireButtons); }

    function wireSwipe(card) {
      var tracking = null;

      card.addEventListener('pointerdown', function (e) {
        if (e.button != null && e.button !== 0) return;
        tracking = {
          id: e.pointerId,
          startX: e.clientX,
          startY: e.clientY,
          x: 0,
          y: 0,
          moved: false
        };
        try { card.setPointerCapture(e.pointerId); } catch (_) {}
      });

      card.addEventListener('pointermove', function (e) {
        if (!tracking || tracking.id !== e.pointerId) return;
        tracking.x = e.clientX - tracking.startX;
        tracking.y = e.clientY - tracking.startY;

        if (!tracking.moved) {
          if (Math.abs(tracking.y) > 12 && Math.abs(tracking.y) > Math.abs(tracking.x)) {
            tracking = null;
            card.style.transform = '';
            return;
          }
          tracking.moved = true;
        }

        var w = Math.max(320, card.getBoundingClientRect().width);
        var pct = clamp(tracking.x / w, -1, 1);
        var rot = pct * 8;
        card.style.transform = 'translateX(' + tracking.x + 'px) rotate(' + rot + 'deg)';
        card.style.opacity = String(1 - Math.min(0.35, Math.abs(pct) * 0.35));
      });

      function endSwipe(e) {
        if (!tracking || tracking.id !== e.pointerId) return;
        var dx = tracking.x;
        tracking = null;

        var w = Math.max(320, card.getBoundingClientRect().width);
        var threshold = Math.max(110, w * 0.22);
        if (Math.abs(dx) >= threshold) {
          card.style.transform = '';
          card.style.opacity = '';
          var dir = dx < 0 ? 'left' : 'right';
          act(dir);
          return;
        }
        card.style.transition = 'transform 120ms ease-out, opacity 120ms ease-out';
        card.style.transform = '';
        card.style.opacity = '';
        setTimeout(function () { card.style.transition = ''; }, 140);
      }

      card.addEventListener('pointerup', endSwipe);
      card.addEventListener('pointercancel', endSwipe);
    }

    function wireSwipeAll() { cards.forEach(wireSwipe); }

    async function maybePrefetchMore() {
      if (!canPaginate) return;
      if (!fragmentBase) return;
      if (prefetchInFlight) return;
      if (remainingCount() > prefetchWhenRemaining) return;

      prefetchInFlight = true;
      try {
        var url = asURL(fragmentBase);
        if (!url) return;
        url.searchParams.set('page', String(nextPage));
        var resp = await fetch(url.toString(), { credentials: 'same-origin' });
        if (!resp.ok) return;
        var html = await resp.text();
        var doc = parseHTML(html);
        var frag = doc.querySelector('[data-review-fragment]');
        if (!frag) return;
        var newCards = Array.prototype.slice.call(frag.querySelectorAll('[data-review-card]'));
        if (newCards.length === 0) return;

        newCards.forEach(function (c) {
          c.hidden = true;
          deck.appendChild(c);
        });
        cards = qsa(deck, '[data-review-card]');
        newCards.forEach(function (c) { wireButtons(c); wireSwipe(c); });
        nextPage = nextPage + 1;
      } finally {
        prefetchInFlight = false;
      }
    }

    async function maybePrefetchDetails(card) {
      var expandUrl = card.getAttribute('data-expand-url');
      var targetSel = card.getAttribute('data-expand-target') || '';
      if (!expandUrl || !targetSel) return;
      if (card.getAttribute('data-expanded') === 'true') return;

      var target = qs(card, targetSel);
      if (!target) return;

      card.setAttribute('data-expanded', 'inflight');
      try {
        var resp = await fetch(expandUrl, { credentials: 'same-origin' });
        if (!resp.ok) { card.setAttribute('data-expanded', 'false'); return; }
        var html = await resp.text();
        var doc = parseHTML(html);
        var frag = doc.querySelector('[data-report-fragment]');
        if (!frag) { card.setAttribute('data-expanded', 'false'); return; }
        target.innerHTML = '';
        target.appendChild(frag);
        target.hidden = false;
        card.setAttribute('data-expanded', 'true');
      } catch (_) {
        card.setAttribute('data-expanded', 'false');
      }
    }

    function wireExpandButtons() {
      cards.forEach(function (card) {
        var btn = qs(card, '[data-review-expand]');
        if (!btn) return;
        btn.addEventListener('click', function (e) {
          e.preventDefault();
          card.setAttribute('data-expanded', 'false');
          maybePrefetchDetails(card);
        });
      });
    }

    deck.addEventListener('keydown', onKeyDown);
    deck.addEventListener('review:keyboard', onKeyboardAction);
    wireAll();
    wireSwipeAll();
    wireExpandButtons();

    requestAnimationFrame(function () {
      ensureActiveCard();
    });
  }

  function init() {
    var decks = document.querySelectorAll('[data-review-deck]');
    for (var i = 0; i < decks.length; i++) {
      enhanceDeck(decks[i]);
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
"
}
