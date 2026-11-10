// SPDX-License-Identifier: AGPL-3.0-or-later

import {captureFrameImages} from '@app/features/boot_state/BootFrameImages';
import type {FrameComposer, HeldFrameInput, replayFrameInput} from '@app/features/boot_state/BootFrameInput';
import {BOOT_FRAME_GLOBAL, BOOT_FRAME_SCROLL_ATTRIBUTE} from '@app/features/boot_state/BootStateBrowserFormat';
import type {ComposerHandle} from '@app/features/lexical/composer/ComposerHandle';
import {BOOT_FRAME_LAYOUT_MOVED_ATTRIBUTE, RELOAD_COVER_ELEMENT_ID} from '@fluxer/desktop_ipc/src/BootStateContract';

export const TRANSIENT_UI_SELECTOR =
	'[data-flx="channel.typing-users.div"], [data-flx^="channel.message-action-bar"], [role="tooltip"]';

const RELOAD_UNDIM_MS = 260;
const STRIPPED_SELECTOR = `script, noscript, ${TRANSIENT_UI_SELECTOR}`;
const INERT_MEDIA_SELECTOR = 'iframe, video, audio, source';
const EDITABLE_SELECTOR = '[contenteditable="true"]';
const UNSTORED_ROOT_CLASSES = new Set(['window-focused']);

export interface DomBootFrame {
	readonly b: string;
	readonly p: string;
	readonly w: number;
	readonly h: number;
	readonly r: number;
	readonly d: boolean | null;
	readonly c: ReadonlyArray<string>;
	readonly s: string;
	readonly l: ReadonlyArray<FrameStylesheet>;
	readonly t: ReadonlyArray<string>;
	readonly f: ReadonlyArray<readonly [string, string, string]>;
	readonly i: ReadonlyArray<string>;
	readonly x: string;
}

type FrameStylesheet = {readonly href: string; readonly integrity: string | null} | {readonly text: string};

interface BootFrameController {
	readonly settled: Promise<void>;
	whenInputSettled(): Promise<void>;
	needsHistory(): boolean;
	release(): HeldFrameInput | null;
	drop(): void;
}

function markScrollPositions(source: HTMLElement, clone: HTMLElement): void {
	const sourceElements = source.querySelectorAll<HTMLElement>('*');
	const cloneElements = clone.querySelectorAll<HTMLElement>('*');
	for (let index = 0; index < sourceElements.length; index += 1) {
		const element = sourceElements[index];
		if (element.scrollTop === 0 && element.scrollLeft === 0 && !element.hasAttribute('data-fluxer-scroll-container')) {
			continue;
		}
		cloneElements[index]?.setAttribute(BOOT_FRAME_SCROLL_ATTRIBUTE, `${element.scrollTop},${element.scrollLeft}`);
	}
}

function trimHistoryAboveViewport(source: HTMLElement, clone: HTMLElement): void {
	const selector = '[data-flx="channel.messages.scroller"]';
	const innerSelector = '[data-flx="channel.messages.scroller-inner"]';
	const scroller = source.querySelector<HTMLElement>(selector);
	const inner = scroller?.querySelector<HTMLElement>(innerSelector);
	const clonedScroller = clone.querySelector<HTMLElement>(selector);
	const clonedInner = clonedScroller?.querySelector<HTMLElement>(innerSelector);
	if (scroller == null || inner == null || clonedScroller == null || clonedInner == null) return;
	const viewport = scroller.getBoundingClientRect();
	const children = Array.from(inner.children);
	const firstKept = children.findIndex(
		(child) => child.getBoundingClientRect().bottom >= viewport.top - viewport.height,
	);
	if (firstKept <= 0) return;
	const removedHeight = children[firstKept].getBoundingClientRect().top - children[0].getBoundingClientRect().top;
	for (const child of Array.from(clonedInner.children).slice(0, firstKept)) child.remove();
	clonedScroller.setAttribute(
		BOOT_FRAME_SCROLL_ATTRIBUTE,
		`${Math.max(0, scroller.scrollTop - removedHeight)},${scroller.scrollLeft}`,
	);
}

function loadedFontFaces(): DomBootFrame['f'] {
	return Array.from(document.fonts)
		.filter((face) => face.status === 'loaded')
		.map((face) => [face.family, face.weight, face.style] as const);
}

function makeInert(clone: HTMLElement): void {
	for (const element of clone.querySelectorAll(STRIPPED_SELECTOR)) {
		element.remove();
	}
	for (const element of clone.querySelectorAll(INERT_MEDIA_SELECTOR)) {
		element.removeAttribute('src');
		element.removeAttribute('srcdoc');
		element.removeAttribute('autoplay');
	}
	for (const image of clone.querySelectorAll('img')) {
		if (image.getAttribute('src')?.startsWith('blob:')) {
			image.removeAttribute('src');
		}
	}
	for (const editable of clone.querySelectorAll(EDITABLE_SELECTOR)) {
		const paragraph = document.createElement('p');
		paragraph.className = editable.firstElementChild?.className ?? '';
		paragraph.append(document.createElement('br'));
		editable.replaceChildren(paragraph);
	}
	for (const input of clone.querySelectorAll('input, textarea')) {
		input.removeAttribute('value');
		input.textContent = '';
	}
	for (const element of clone.querySelectorAll('[autofocus]')) {
		element.removeAttribute('autofocus');
	}
}

function stylesheetLinks(): Array<HTMLLinkElement> {
	return Array.from(document.querySelectorAll<HTMLLinkElement>('link[rel="stylesheet"][href]'));
}

function captureStylesheets(links: ReadonlyArray<HTMLLinkElement>): DomBootFrame['l'] {
	return links.map((link) => ({href: link.getAttribute('href') ?? '', integrity: link.getAttribute('integrity')}));
}

export async function captureDomBootFrame(systemDark: boolean | null): Promise<string | null> {
	const root = document.getElementById('root');
	if (root === null || root.firstElementChild === null) {
		return null;
	}
	const clone = root.cloneNode(true) as HTMLElement;
	markScrollPositions(root, clone);
	const images = await captureFrameImages(root, clone, () => {
		trimHistoryAboveViewport(root, clone);
		makeInert(clone);
	});
	if (images === null) return null;
	const frame: DomBootFrame = {
		b: import.meta.env.BUILD_VERSION ?? 'dev',
		p: location.pathname,
		w: window.innerWidth,
		h: window.innerHeight,
		r: window.devicePixelRatio,
		d: systemDark,
		c: Array.from(document.documentElement.classList).filter((name) => !UNSTORED_ROOT_CLASSES.has(name)),
		s: document.documentElement.getAttribute('style') ?? '',
		l: captureStylesheets(stylesheetLinks()),
		t: Array.from(document.head.querySelectorAll('style'), (style) => style.textContent ?? ''),
		f: loadedFontFaces(),
		i: images,
		x: clone.innerHTML,
	};
	return JSON.stringify(frame);
}

function takeFrameController(): BootFrameController | null {
	const holder = window as unknown as Record<string, BootFrameController | undefined>;
	const controller = holder[BOOT_FRAME_GLOBAL] ?? null;
	delete holder[BOOT_FRAME_GLOBAL];
	return controller;
}

let composer: FrameComposer | null = null;

export function registerBootFrameComposer(
	resolve: () => ComposerHandle | null,
	adopt: (text: string) => void,
): () => void {
	const registration = {resolve, adopt};
	composer = registration;
	return () => {
		if (composer === registration) composer = null;
	};
}

export function bootFrameNeedsHistory(): boolean {
	const holder = window as unknown as Record<string, BootFrameController | undefined>;
	return holder[BOOT_FRAME_GLOBAL]?.needsHistory() === true;
}

export function whenBootFrameInputSettled(): Promise<void> {
	const holder = window as unknown as Record<string, BootFrameController | undefined>;
	const controller = holder[BOOT_FRAME_GLOBAL];
	return controller === undefined ? Promise.resolve() : controller.whenInputSettled();
}

type ContentPaintedArgs = [view: 'other'] | [view: 'restored', replay: typeof replayFrameInput];

function releaseDomBootFrame(...args: ContentPaintedArgs): void {
	const root = document.documentElement;
	const layoutMoved = root.hasAttribute(BOOT_FRAME_LAYOUT_MOVED_ATTRIBUTE);
	root.removeAttribute(BOOT_FRAME_LAYOUT_MOVED_ATTRIBUTE);
	const controller = takeFrameController();
	if (controller === null) {
		return;
	}
	if (args[0] !== 'restored') {
		controller.drop();
		return;
	}
	const input = controller.release();
	if (input === null) {
		return;
	}
	args[1](input, () => composer, layoutMoved);
}

export function undimAfterReload(): void {
	const cover = document.getElementById(RELOAD_COVER_ELEMENT_ID);
	if (cover === null) return;
	if (matchMedia('(prefers-reduced-motion: reduce)').matches) {
		cover.remove();
		return;
	}
	cover.style.filter = getComputedStyle(cover).filter;
	cover.style.animation = 'none';
	cover.style.transition = `opacity ${RELOAD_UNDIM_MS}ms ease-in`;
	requestAnimationFrame(() => {
		cover.style.opacity = '0';
		setTimeout(() => cover.remove(), RELOAD_UNDIM_MS + 50);
	});
}

export function notifyFirstContentPainted(...args: ContentPaintedArgs): void {
	if (args[0] === 'restored') {
		window.electron?.notifyFirstContentPainted?.('restored');
	} else {
		window.electron?.notifyFirstContentPainted?.();
	}
	releaseDomBootFrame(...args);
	undimAfterReload();
}
