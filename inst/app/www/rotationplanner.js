// コート図のドラッグ操作と、試合中モードのキーボードショートカット
(function () {
  'use strict';

  // ---- ドラッグ -------------------------------------------------------------
  // 自チーム(data-side="us")は下側、相手(data-side="them")は上側に 180° 回して描いている。
  // コートは遠近法(台形)で描いているので、画面座標 ⇔ 表示座標 を data-persp の値で変換する。
  // 送る座標は、それぞれのチームの自コート視点(x: 左→右, y: ネット→エンドライン)。
  let drag = null;
  const clamp = (v, lo, hi) => Math.min(hi, Math.max(lo, v));

  function svgPoint(svg, evt) {
    const ctm = svg.getScreenCTM();
    if (!ctm) return null;
    return new DOMPoint(evt.clientX, evt.clientY).matrixTransform(ctm.inverse());
  }

  function perspective(svg) {
    const p = (svg.dataset.persp || '').split(',').map(Number);
    return p.length === 4 ? { cx: p[0], H: p[1], z0: p[2], yh: p[3] } : null;
  }

  // 表示座標 → 画面座標(R の project_point() と同じ式)
  function project(pr, x, y) {
    if (!pr) return { x: x, y: y };
    const w = pr.z0 / (pr.z0 + (pr.H - y));
    return { x: pr.cx + (x - pr.cx) * w, y: pr.yh + (pr.H - pr.yh) * w };
  }

  // 画面座標 → 表示座標
  function unproject(pr, X, Y) {
    if (!pr) return { x: X, y: Y };
    const w = Math.max((Y - pr.yh) / (pr.H - pr.yh), 0.05);
    return { x: pr.cx + (X - pr.cx) / w, y: pr.H - (pr.z0 / w - pr.z0) };
  }

  document.addEventListener('pointerdown', function (evt) {
    const g = evt.target.closest && evt.target.closest('.rp-court .marker.draggable');
    if (!g) return;
    drag = { g: g, svg: g.ownerSVGElement, point: null };
    g.classList.add('dragging');
    evt.preventDefault();
  });

  document.addEventListener('pointermove', function (evt) {
    if (!drag) return;
    const pt = svgPoint(drag.svg, evt);
    if (!pt) return;
    const pr = perspective(drag.svg);
    const offset = Number(drag.svg.dataset.offset || 0);
    const them = drag.g.dataset.side === 'them';
    const disp = unproject(pr, pt.x, pt.y);
    // チームの自コート座標に直して範囲を制限し、表示座標 → 画面座標に戻す
    const x = clamp(them ? 9 - disp.x : disp.x, -0.5, 9.5);
    const y = clamp(them ? 9 - disp.y : disp.y - offset, 0, 10.5);
    drag.point = { x: x, y: y };
    const scr = project(pr, them ? 9 - x : x, them ? 9 - y : y + offset);
    drag.g.setAttribute('transform', 'translate(' + scr.x.toFixed(2) + ' ' + scr.y.toFixed(2) + ')');
  });

  document.addEventListener('pointerup', function () {
    if (!drag) return;
    const d = drag;
    drag = null;
    d.g.classList.remove('dragging');
    if (!d.point || !window.Shiny) return;
    Shiny.setInputValue(d.svg.dataset.input, {
      court: d.svg.dataset.court || null,
      side: d.g.dataset.side,
      rot: d.g.dataset.rot === undefined ? null : Number(d.g.dataset.rot),
      phase: d.g.dataset.phase || null,
      pos: Number(d.g.dataset.pos),
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
