// コート図のドラッグ操作と、試合中モードのキーボードショートカット
(function () {
  'use strict';

  // ---- ドラッグ -------------------------------------------------------------
  let drag = null;

  function toCourt(svg, evt) {
    const ctm = svg.getScreenCTM();
    if (!ctm) return null;
    const pt = new DOMPoint(evt.clientX, evt.clientY).matrixTransform(ctm.inverse());
    const offset = Number(svg.dataset.offset || 0);
    const clamp = (v, lo, hi) => Math.min(hi, Math.max(lo, v));
    return { x: clamp(pt.x, -0.5, 9.5), y: clamp(pt.y - offset, 0, 10.5), svgY: pt.y };
  }

  document.addEventListener('pointerdown', function (evt) {
    const g = evt.target.closest && evt.target.closest('.rp-court .marker.draggable');
    if (!g) return;
    const svg = g.ownerSVGElement;
    drag = { g: g, svg: svg, pos: Number(g.dataset.pos), point: null };
    g.classList.add('dragging');
    evt.preventDefault();
  });

  document.addEventListener('pointermove', function (evt) {
    if (!drag) return;
    const p = toCourt(drag.svg, evt);
    if (!p) return;
    drag.point = p;
    drag.g.setAttribute('transform', 'translate(' + p.x.toFixed(2) + ' ' + (p.y + Number(drag.svg.dataset.offset || 0)).toFixed(2) + ')');
  });

  document.addEventListener('pointerup', function () {
    if (!drag) return;
    const d = drag;
    drag = null;
    d.g.classList.remove('dragging');
    if (!d.point || !window.Shiny) return;
    Shiny.setInputValue(d.svg.dataset.input, {
      court: d.svg.dataset.court || null,
      pos: d.pos,
      x: Math.round(d.point.x * 10) / 10,
      y: Math.round(d.point.y * 10) / 10,
    }, { priority: 'event' });
  });

  // ---- キーボード(試合中モード) ---------------------------------------------
  // A = 自チーム得点, L = 相手得点, Z = 取り消し
  const KEYS = { a: 'us', l: 'them', z: 'undo' };

  document.addEventListener('keydown', function (evt) {
    if (evt.ctrlKey || evt.metaKey || evt.altKey || evt.repeat) return;
    const tag = (evt.target.tagName || '').toLowerCase();
    if (tag === 'input' || tag === 'textarea' || tag === 'select' || evt.target.isContentEditable) return;
    const action = KEYS[evt.key.toLowerCase()];
    if (!action || !window.Shiny) return;
    // 表示中の試合画面だけに送る
    const target = Array.from(document.querySelectorAll('.rp-live-keys')).find((el) => el.offsetParent !== null);
    if (!target) return;
    evt.preventDefault();
    Shiny.setInputValue(target.dataset.input, action, { priority: 'event' });
  });
})();
