// SPDX-License-Identifier: AGPL-3.0-or-later

import {APP_HIGHLIGHT_ATTRIBUTE} from '@app/features/theme_settings/editor/AppHighlights';
import {SEMANTIC_ATTRIBUTES} from '@app/features/theme_settings/editor/AppSelectors';
import {indentUnit, syntaxTree} from '@codemirror/language';
import type {EditorState} from '@codemirror/state';
import type {EditorView} from '@codemirror/view';
import {observable, runInAction} from 'mobx';

type SyntaxNode = ReturnType<typeof syntaxTree>['topNode'];

const BLOCKED_EVENTS = [
	'pointerdown',
	'pointerup',
	'mousedown',
	'mouseup',
	'click',
	'dblclick',
	'auxclick',
	'contextmenu',
];
const IGNORED_SELECTOR = `[data-quick-css-panel], [${APP_HIGHLIGHT_ATTRIBUTE}]`;

const HOST_STYLE = [
	'position: fixed !important',
	'inset: 0 !important',
	'width: 100vw !important',
	'height: 100vh !important',
	'margin: 0 !important',
	'padding: 0 !important',
	'border: none !important',
	'background: transparent !important',
	'overflow: visible !important',
	'pointer-events: none !important',
	'display: block !important',
	'visibility: visible !important',
	'opacity: 1 !important',
].join(';');

const BOX_STYLE = [
	'position: absolute',
	'box-sizing: border-box',
	'border: 2px solid var(--brand-primary, #4641d9)',
	'background: color-mix(in srgb, var(--brand-primary, #4641d9) 18%, transparent)',
	'border-radius: 2px',
	'display: none',
].join(';');

const LABEL_STYLE = [
	'position: absolute',
	'max-width: min(560px, calc(100vw - 16px))',
	'padding: 4px 8px',
	'border-radius: 4px',
	'background: var(--brand-primary, #4641d9)',
	'color: var(--brand-primary-fill, #ffffff)',
	'font: 500 12px/1.4 var(--font-mono, ui-monospace, monospace)',
	'white-space: nowrap',
	'overflow: hidden',
	'text-overflow: ellipsis',
	'display: none',
].join(';');

const HINT_STYLE = [
	'position: absolute',
	'bottom: 24px',
	'left: calc(50% + (var(--quick-css-dock-left, 0px) - var(--quick-css-dock-right, 0px)) / 2)',
	'transform: translateX(-50%)',
	'max-width: calc(100vw - 32px - var(--quick-css-dock-left, 0px) - var(--quick-css-dock-right, 0px))',
	'padding: 8px 12px',
	'border-radius: 8px',
	'background: var(--background-floating, #111214)',
	'color: var(--text-primary, #ffffff)',
	'box-shadow: 0 8px 24px rgba(0, 0, 0, 0.32)',
	'font: 500 14px/1.4 var(--font-sans, system-ui, sans-serif)',
	'text-align: center',
].join(';');

interface PickerSession {
	view: EditorView;
	host: HTMLDivElement;
	box: HTMLDivElement;
	label: HTMLDivElement;
	target: Element | null;
	children: Array<Element>;
	frame: number | null;
	dispose: () => void;
}

const lastPointer = {x: -1, y: -1};
let session: PickerSession | null = null;
const active = observable.box(false);

export function isElementPickerActive(): boolean {
	return active.get();
}

export function trackLastPointer(): () => void {
	const handlePointerMove = (event: PointerEvent): void => {
		lastPointer.x = event.clientX;
		lastPointer.y = event.clientY;
	};
	window.addEventListener('pointermove', handlePointerMove, {capture: true, passive: true});
	return () => window.removeEventListener('pointermove', handlePointerMove, {capture: true});
}

function escapeAttributeValue(value: string): string {
	return value.replace(/["\\]/g, (character) => `\\${character}`);
}

function elementSelector(element: Element, withSemanticAttributes: boolean): string {
	let selector = `[data-flx="${escapeAttributeValue(element.getAttribute('data-flx') ?? '')}"]`;
	if (withSemanticAttributes) {
		for (const attribute of SEMANTIC_ATTRIBUTES) {
			if (element.hasAttribute(attribute.name)) selector += `[${attribute.name}]`;
		}
	}
	return selector;
}

function insideSelector(state: EditorState, pos: number): boolean {
	for (let node: SyntaxNode | null = syntaxTree(state).resolveInner(pos, -1); node; node = node.parent) {
		if (node.name === 'Declaration') return false;
		if (node.name === 'Block') break;
		if (node.name === 'RuleSet') return true;
	}
	const line = state.doc.lineAt(pos);
	const before = state.sliceDoc(line.from, pos).trimEnd();
	return before.length > 0 && !/[{};]$/.test(before);
}

function insertSelector(view: EditorView, selector: string): void {
	const {state} = view;
	const range = state.selection.main;
	if (insideSelector(state, range.from)) {
		view.dispatch({
			changes: {from: range.from, to: range.to, insert: selector},
			selection: {anchor: range.from + selector.length},
			scrollIntoView: true,
			userEvent: 'input.pick',
		});
		return;
	}
	const line = state.doc.lineAt(range.from);
	const lineIndent = /^\s*/.exec(line.text)?.[0] ?? '';
	const atLineStart = state.sliceDoc(line.from, range.from).trim().length === 0;
	const indent = atLineStart ? state.sliceDoc(line.from, range.from) : lineIndent;
	const opening = `${selector} {\n${indent}${state.facet(indentUnit)}`;
	const insert = `${atLineStart ? '' : `\n${indent}`}${opening}\n${indent}}`;
	view.dispatch({
		changes: {from: range.from, to: range.to, insert},
		selection: {anchor: range.from + (atLineStart ? 0 : indent.length + 1) + opening.length},
		scrollIntoView: true,
		userEvent: 'input.pick',
	});
}

function nearestFlxElement(target: EventTarget | null): Element | null {
	if (!(target instanceof Element) || target.closest(IGNORED_SELECTOR)) return null;
	return target.closest('[data-flx]');
}

function parentFlxElement(element: Element): Element | null {
	return element.parentElement?.closest('[data-flx]') ?? null;
}

function render(current: PickerSession): void {
	current.frame = null;
	const target = current.target;
	const rect = target?.isConnected ? target.getBoundingClientRect() : null;
	if (!target || !rect || (rect.width === 0 && rect.height === 0)) {
		current.box.style.display = 'none';
		current.label.style.display = 'none';
		return;
	}
	Object.assign(current.box.style, {
		display: 'block',
		left: `${rect.left}px`,
		top: `${rect.top}px`,
		width: `${rect.width}px`,
		height: `${rect.height}px`,
	});
	current.label.textContent = elementSelector(target, false);
	current.label.style.display = 'block';
	const labelHeight = current.label.offsetHeight;
	const top =
		rect.top - labelHeight - 4 >= 0 ? rect.top - labelHeight - 4 : Math.min(rect.bottom + 4, innerHeight - labelHeight);
	current.label.style.left = `${Math.max(8, Math.min(rect.left, innerWidth - current.label.offsetWidth - 8))}px`;
	current.label.style.top = `${Math.max(0, top)}px`;
}

function scheduleRender(current: PickerSession): void {
	if (current.frame === null) current.frame = requestAnimationFrame(() => render(current));
}

function setTarget(current: PickerSession, target: Element | null, children: Array<Element> = []): void {
	if (current.target === target) return;
	current.target = target;
	current.children = children;
	scheduleRender(current);
}

export function stopElementPicker(): void {
	const current = session;
	if (!current) return;
	session = null;
	current.dispose();
	runInAction(() => active.set(false));
}

export function stopElementPickerFor(view: EditorView): void {
	if (session?.view === view) stopElementPicker();
}

interface ElementPickerOptions {
	hint: string;
	onDone: () => void;
}

export function startElementPicker(view: EditorView, {hint, onDone}: ElementPickerOptions): void {
	stopElementPicker();
	const host = document.createElement('div');
	host.setAttribute(APP_HIGHLIGHT_ATTRIBUTE, '');
	host.setAttribute('popover', 'manual');
	host.style.cssText = HOST_STYLE;
	const box = document.createElement('div');
	box.style.cssText = BOX_STYLE;
	const label = document.createElement('div');
	label.style.cssText = LABEL_STYLE;
	label.setAttribute('aria-hidden', 'true');
	const hintElement = document.createElement('div');
	hintElement.style.cssText = HINT_STYLE;
	hintElement.setAttribute('role', 'status');
	hintElement.textContent = hint;
	host.append(box, label, hintElement);
	document.documentElement.appendChild(host);
	host.showPopover();
	const editorWindow = view.dom.ownerDocument.defaultView;
	const finish = (): void => {
		stopElementPicker();
		onDone();
	};
	const pick = (withSemanticAttributes: boolean): void => {
		const target = current.target;
		if (!target) return;
		insertSelector(view, elementSelector(target, withSemanticAttributes));
		finish();
	};
	const handleBlockedEvent = (event: Event): void => {
		if (event.target instanceof Element && event.target.closest(IGNORED_SELECTOR)) return;
		event.preventDefault();
		event.stopImmediatePropagation();
		if (event.type !== 'click') return;
		const target = nearestFlxElement(event.target);
		if (target && target !== current.target && !current.target?.contains(target)) setTarget(current, target);
		pick((event as MouseEvent).shiftKey);
	};
	const handlePointerMove = (event: PointerEvent): void => {
		const target = nearestFlxElement(event.target);
		if (target) setTarget(current, target);
	};
	const handleKeyDown = (event: KeyboardEvent): void => {
		const mod = event.metaKey || event.ctrlKey;
		if (event.key === 'Escape' || (mod && event.shiftKey && event.key.toLowerCase() === 'c')) {
			event.preventDefault();
			event.stopImmediatePropagation();
			finish();
			return;
		}
		if (event.altKey && (event.key === 'ArrowUp' || event.key === 'ArrowDown')) {
			event.preventDefault();
			event.stopImmediatePropagation();
			const target = current.target;
			if (!target) return;
			if (event.key === 'ArrowUp') {
				const parent = parentFlxElement(target);
				if (parent) setTarget(current, parent, [...current.children, target]);
				return;
			}
			const children = [...current.children];
			const child = children.pop();
			if (child?.isConnected) setTarget(current, child, children);
			return;
		}
		if (event.key === 'Enter' && current.target) {
			event.preventDefault();
			event.stopImmediatePropagation();
			pick(event.shiftKey);
		}
	};
	const handleViewportChange = (): void => scheduleRender(current);
	const windows = editorWindow && editorWindow !== window ? [window, editorWindow] : [window];
	for (const type of BLOCKED_EVENTS) window.addEventListener(type, handleBlockedEvent, true);
	window.addEventListener('pointermove', handlePointerMove, true);
	window.addEventListener('scroll', handleViewportChange, true);
	window.addEventListener('resize', handleViewportChange);
	for (const target of windows) target.addEventListener('keydown', handleKeyDown, true);
	const current: PickerSession = {
		view,
		host,
		box,
		label,
		target: null,
		children: [],
		frame: null,
		dispose: () => {
			for (const type of BLOCKED_EVENTS) window.removeEventListener(type, handleBlockedEvent, true);
			window.removeEventListener('pointermove', handlePointerMove, true);
			window.removeEventListener('scroll', handleViewportChange, true);
			window.removeEventListener('resize', handleViewportChange);
			for (const target of windows) target.removeEventListener('keydown', handleKeyDown, true);
			if (current.frame !== null) cancelAnimationFrame(current.frame);
			host.remove();
		},
	};
	session = current;
	runInAction(() => active.set(true));
	const x = lastPointer.x >= 0 ? lastPointer.x : innerWidth / 2;
	const y = lastPointer.y >= 0 ? lastPointer.y : innerHeight / 2;
	setTarget(current, nearestFlxElement(document.elementFromPoint(x, y)));
}
