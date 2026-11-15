// SPDX-License-Identifier: AGPL-3.0-or-later

import {Logger} from '@app/features/platform/utils/AppLogger';
import styles from '@app/features/ui/popout_window/PopoutWindow.module.css';
import {
	UNFOCUSED_FULLY_INTERACTIVE_CLASS,
	WINDOW_FOCUS_ACTIVATION_GUARD_CLASS,
	WINDOW_FOCUSED_CLASS,
} from '@app/features/ui/utils/WindowFocusInteractionGuard';

const logger = new Logger('PopoutWindowDocument');

const POPOUT_STYLESHEET_COPY_MAX = 256;
const POPOUT_THEME_ATTRIBUTE_COPY_MAX = 64;
const POPOUT_THEME_CLASS_COPY_MAX = 128;
const MIRROR_NODE_ATTRIBUTE = 'data-popout-mirror';

const POPOUT_OWNED_ROOT_CLASSES = new Set([
	WINDOW_FOCUSED_CLASS,
	WINDOW_FOCUS_ACTIVATION_GUARD_CLASS,
	UNFOCUSED_FULLY_INTERACTIVE_CLASS,
]);

function splitClassNames(value: string): Array<string> {
	const trimmedValue = value.trim();
	if (!trimmedValue) return [];
	return trimmedValue.split(/\s+/).slice(0, POPOUT_THEME_CLASS_COPY_MAX);
}

function syncRootClassAttribute(sourceValue: string, targetRoot: HTMLElement): void {
	const copiedClassNames = splitClassNames(sourceValue).filter(
		(className) => !POPOUT_OWNED_ROOT_CLASSES.has(className),
	);
	const ownedClassNames = splitClassNames(targetRoot.getAttribute('class') ?? '').filter((className) =>
		POPOUT_OWNED_ROOT_CLASSES.has(className),
	);
	const nextClassName = Array.from(new Set([...copiedClassNames, ...ownedClassNames])).join(' ');
	if (nextClassName) {
		targetRoot.setAttribute('class', nextClassName);
		return;
	}
	targetRoot.removeAttribute('class');
}

interface DocumentStylesheetMirror {
	mirroredLinks: WeakMap<Node, HTMLLinkElement>;
	mirroredStyles: WeakMap<Node, HTMLStyleElement>;
}

export type StylesheetExclude = (node: Element) => boolean;

interface StylesheetMirrorOptions {
	exclude?: StylesheetExclude;
}

const documentMirrors = new WeakMap<Document, DocumentStylesheetMirror>();

function getDocumentMirror(targetDocument: Document): DocumentStylesheetMirror {
	const existing = documentMirrors.get(targetDocument);
	if (existing) return existing;
	const stylesheetMirror: DocumentStylesheetMirror = {mirroredLinks: new WeakMap(), mirroredStyles: new WeakMap()};
	documentMirrors.set(targetDocument, stylesheetMirror);
	return stylesheetMirror;
}

function inlineStylesheetFallback(sourceLink: HTMLLinkElement, targetDocument: Document): HTMLStyleElement | null {
	let cssText = '';
	try {
		const rules = sourceLink.sheet?.cssRules;
		if (!rules) return null;
		const ruleTexts: Array<string> = [];
		for (let index = 0; index < rules.length; index += 1) {
			ruleTexts.push(rules[index].cssText);
		}
		cssText = ruleTexts.join('\n');
	} catch (error) {
		logger.warn('Failed to inline popout stylesheet fallback', {href: sourceLink.href, error});
		return null;
	}
	const styleElement = targetDocument.createElement('style');
	styleElement.setAttribute(MIRROR_NODE_ATTRIBUTE, '');
	styleElement.textContent = cssText;
	targetDocument.head.appendChild(styleElement);
	return styleElement;
}

function copyLinkStylesheet(sourceLink: HTMLLinkElement, targetDocument: Document): void {
	const stylesheetMirror = getDocumentMirror(targetDocument);
	const linkElement = targetDocument.createElement('link');
	linkElement.setAttribute(MIRROR_NODE_ATTRIBUTE, '');
	linkElement.rel = 'stylesheet';
	linkElement.href = sourceLink.href;
	if (sourceLink.media) {
		linkElement.media = sourceLink.media;
	}
	linkElement.addEventListener('error', () => {
		linkElement.remove();
		stylesheetMirror.mirroredLinks.delete(sourceLink);
		const fallbackStyle = inlineStylesheetFallback(sourceLink, targetDocument);
		if (fallbackStyle) {
			stylesheetMirror.mirroredStyles.set(sourceLink, fallbackStyle);
		}
	});
	stylesheetMirror.mirroredLinks.set(sourceLink, linkElement);
	targetDocument.head.appendChild(linkElement);
}

function copyStyleElement(sourceStyle: HTMLStyleElement, targetDocument: Document): void {
	const styleElement = targetDocument.createElement('style');
	styleElement.setAttribute(MIRROR_NODE_ATTRIBUTE, '');
	styleElement.textContent = sourceStyle.textContent;
	getDocumentMirror(targetDocument).mirroredStyles.set(sourceStyle, styleElement);
	targetDocument.head.appendChild(styleElement);
}

function removeMirroredStylesheet(stylesheetMirror: DocumentStylesheetMirror, node: Node): void {
	const mirroredStyle = stylesheetMirror.mirroredStyles.get(node);
	if (mirroredStyle) {
		mirroredStyle.remove();
		stylesheetMirror.mirroredStyles.delete(node);
	}
	const mirroredLink = stylesheetMirror.mirroredLinks.get(node);
	if (mirroredLink) {
		mirroredLink.remove();
		stylesheetMirror.mirroredLinks.delete(node);
	}
}

function copyStylesheetsIntoDocument(
	sourceDocument: Document,
	targetDocument: Document,
	{exclude}: StylesheetMirrorOptions = {},
): void {
	const styleNodes = sourceDocument.querySelectorAll('link[rel="stylesheet"], style');
	const copyCount = Math.min(styleNodes.length, POPOUT_STYLESHEET_COPY_MAX);
	for (let index = 0; index < copyCount; index += 1) {
		const node = styleNodes[index];
		if (exclude?.(node)) {
			continue;
		}
		if (node instanceof HTMLLinkElement) {
			copyLinkStylesheet(node, targetDocument);
			continue;
		}
		if (node instanceof HTMLStyleElement) {
			copyStyleElement(node, targetDocument);
		}
	}
}

function observeDocumentStylesheets(
	sourceDocument: Document,
	targetDocument: Document,
	{exclude}: StylesheetMirrorOptions = {},
): () => void {
	if (typeof MutationObserver === 'undefined') {
		return () => undefined;
	}
	const stylesheetMirror = getDocumentMirror(targetDocument);
	const observer = new MutationObserver((mutations) => {
		for (const mutation of mutations) {
			if (mutation.type === 'characterData' || mutation.target instanceof HTMLStyleElement) {
				const sourceStyle =
					mutation.target instanceof HTMLStyleElement ? mutation.target : mutation.target.parentElement;
				if (sourceStyle instanceof HTMLStyleElement) {
					if (exclude?.(sourceStyle)) {
						removeMirroredStylesheet(stylesheetMirror, sourceStyle);
					} else {
						const mirroredStyle = stylesheetMirror.mirroredStyles.get(sourceStyle);
						if (mirroredStyle) {
							mirroredStyle.textContent = sourceStyle.textContent;
						}
					}
				}
			}
			for (const node of Array.from(mutation.addedNodes)) {
				if (node instanceof Element && exclude?.(node)) {
					continue;
				}
				if (node instanceof HTMLLinkElement && node.rel === 'stylesheet') {
					copyLinkStylesheet(node, targetDocument);
					continue;
				}
				if (node instanceof HTMLStyleElement) {
					copyStyleElement(node, targetDocument);
				}
			}
			for (const node of Array.from(mutation.removedNodes)) {
				removeMirroredStylesheet(stylesheetMirror, node);
			}
		}
	});
	observer.observe(sourceDocument.head, {childList: true, subtree: true, characterData: true});
	return () => observer.disconnect();
}

function syncDocumentThemeAttributes(sourceDocument: Document, targetDocument: Document): void {
	const sourceRoot = sourceDocument.documentElement;
	const targetRoot = targetDocument.documentElement;
	if (!sourceRoot || !targetRoot) return;
	const seenNames = new Set<string>();
	const attributes = sourceRoot.attributes;
	const copyCount = Math.min(attributes.length, POPOUT_THEME_ATTRIBUTE_COPY_MAX);
	for (let index = 0; index < copyCount; index += 1) {
		const attribute = attributes[index];
		seenNames.add(attribute.name);
		if (attribute.name === 'class') {
			syncRootClassAttribute(attribute.value, targetRoot);
			continue;
		}
		targetRoot.setAttribute(attribute.name, attribute.value);
	}
	for (const existing of Array.from(targetRoot.attributes)) {
		if (!seenNames.has(existing.name)) {
			if (existing.name === 'class') {
				syncRootClassAttribute('', targetRoot);
				continue;
			}
			targetRoot.removeAttribute(existing.name);
		}
	}
}

function observeDocumentThemeAttributes(sourceDocument: Document, targetDocument: Document): () => void {
	if (typeof MutationObserver === 'undefined') {
		return () => undefined;
	}
	const observer = new MutationObserver(() => {
		syncDocumentThemeAttributes(sourceDocument, targetDocument);
	});
	observer.observe(sourceDocument.documentElement, {attributes: true});
	return () => observer.disconnect();
}

interface MirroredDocumentOptions {
	title: string;
	exclude?: StylesheetExclude;
}

export interface MirroredDocument {
	container: HTMLElement;
	dispose: () => void;
}

export function mountMirroredDocument(
	targetWindow: Window,
	{title, exclude}: MirroredDocumentOptions,
): MirroredDocument {
	const targetDocument = targetWindow.document;
	targetDocument.body.replaceChildren();
	for (const node of Array.from(targetDocument.head.querySelectorAll(`[${MIRROR_NODE_ATTRIBUTE}]`))) {
		node.remove();
	}
	documentMirrors.delete(targetDocument);
	targetDocument.title = title;
	syncDocumentThemeAttributes(document, targetDocument);
	targetDocument.documentElement.classList.add(UNFOCUSED_FULLY_INTERACTIVE_CLASS);
	copyStylesheetsIntoDocument(document, targetDocument, {exclude});
	targetDocument.body.style.margin = '0';
	const container = targetDocument.createElement('div');
	container.className = styles.root;
	targetDocument.body.appendChild(container);
	const disconnectThemeObserver = observeDocumentThemeAttributes(document, targetDocument);
	const disconnectStylesheetObserver = observeDocumentStylesheets(document, targetDocument, {exclude});
	return {
		container,
		dispose: () => {
			disconnectThemeObserver();
			disconnectStylesheetObserver();
		},
	};
}
