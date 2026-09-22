// SPDX-License-Identifier: AGPL-3.0-or-later

const ELEMENT_NODE = 1;
const TEXT_NODE = 3;

export const TRANSLATION_MARKER = '~';

const NEVER_TRANSLATED_TAGS = new Set([
	'SCRIPT',
	'STYLE',
	'BR',
	'WBR',
	'CODE',
	'KBD',
	'MAP',
	'OBJECT',
	'PARAM',
	'RP',
	'svg',
]);
const VALUE_ONLY_TAGS = new Set(['INPUT', 'TEXTAREA', 'TITLE']);
const BLOCK_CONTAINER_TAGS = new Set([
	'UL',
	'OL',
	'DL',
	'TABLE',
	'THEAD',
	'TBODY',
	'TFOOT',
	'TR',
	'SELECT',
	'OPTGROUP',
	'MENU',
	'NAV',
	'HEADER',
	'FOOTER',
	'SECTION',
	'ARTICLE',
	'ASIDE',
	'MAIN',
	'FORM',
	'FIELDSET',
	'BODY',
	'HTML',
]);
const INLINE_TAGS = new Set([
	'A',
	'ABBR',
	'ACRONYM',
	'B',
	'BASEFONT',
	'BDO',
	'BIG',
	'CITE',
	'DFN',
	'EM',
	'FONT',
	'I',
	'LABEL',
	'NOBR',
	'Q',
	'S',
	'SMALL',
	'SPAN',
	'STRIKE',
	'STRONG',
	'SUB',
	'SUP',
	'TT',
	'U',
	'VAR',
]);

function isElement(node: Node): node is Element {
	return node.nodeType === ELEMENT_NODE;
}

function isNonBlankText(node: Node): boolean {
	return node.nodeType === TEXT_NODE && (node.nodeValue?.trim().length ?? 0) > 0;
}

export function isTranslationOptedOut(element: Element): boolean {
	return (
		NEVER_TRANSLATED_TAGS.has(element.tagName) ||
		element.classList.contains('notranslate') ||
		element.classList.contains('skiptranslate') ||
		element.getAttribute('translate') === 'no'
	);
}

export function isInlineElement(element: Element): boolean {
	const view = element.ownerDocument.defaultView;
	const display = view?.getComputedStyle ? view.getComputedStyle(element).display : '';
	if (display === 'inline' || display === 'contents') {
		return true;
	}
	return display === '' && INLINE_TAGS.has(element.tagName);
}

export function isCompoundRoot(element: Element): boolean {
	if (VALUE_ONLY_TAGS.has(element.tagName) || BLOCK_CONTAINER_TAGS.has(element.tagName)) {
		return false;
	}
	if (element.tagName === 'PRE' || element.tagName === 'RUBY') {
		return false;
	}
	if (isInlineElement(element) || isTranslationOptedOut(element)) {
		return false;
	}
	let hasInlineContent = false;
	for (const child of Array.from(element.childNodes)) {
		if (isElement(child)) {
			if (isTranslationOptedOut(child)) {
				continue;
			}
			if (!isInlineElement(child)) {
				return false;
			}
			hasInlineContent = true;
		} else if (isNonBlankText(child)) {
			hasInlineContent = true;
		}
	}
	return hasInlineContent;
}

function walkTranslatable(element: Element, visit: (element: Element) => boolean): void {
	if (isTranslationOptedOut(element)) {
		return;
	}
	if (!visit(element)) {
		return;
	}
	for (const child of Array.from(element.childNodes)) {
		if (isElement(child) && child.tagName !== 'FONT') {
			walkTranslatable(child, visit);
		}
	}
}

function translatedTextNode(document: Document, source: Node): Text {
	return document.createTextNode(TRANSLATION_MARKER + (source.nodeValue ?? ''));
}

function reassembledFragment(element: Element, children: ReadonlyArray<Node>): DocumentFragment {
	const document = element.ownerDocument;
	const fragment = document.createDocumentFragment();
	for (const child of children) {
		fragment.appendChild(child.nodeType === TEXT_NODE ? translatedTextNode(document, child) : child.cloneNode(true));
	}
	return fragment;
}

function wrapTextNodesInFont(element: Element): void {
	const document = element.ownerDocument;
	for (const child of Array.from(element.childNodes)) {
		if (child.nodeType !== TEXT_NODE || !isNonBlankText(child)) {
			continue;
		}
		const outer = document.createElement('font');
		const inner = document.createElement('font');
		outer.setAttribute('style', 'vertical-align: inherit;');
		inner.setAttribute('style', 'vertical-align: inherit;');
		outer.appendChild(inner);
		element.insertBefore(outer, child);
		child.nodeValue = TRANSLATION_MARKER + (child.nodeValue ?? '');
		inner.appendChild(child);
	}
}

export function chromeLegacyFontPipeline(root: Element): void {
	walkTranslatable(root, (element) => {
		wrapTextNodesInFont(element);
		return true;
	});
}

export function chromeCompoundPipeline(root: Element): void {
	walkTranslatable(root, (element) => {
		const children = Array.from(element.childNodes);
		const hasText = children.some(isNonBlankText);
		const hasElement = children.some(isElement);
		if (!hasText || !hasElement) {
			return true;
		}
		element.replaceChildren(reassembledFragment(element, children));
		return false;
	});
}

export function chromeFaithfulCompoundPipeline(root: Element): void {
	walkTranslatable(root, (element) => {
		if (!isCompoundRoot(element)) {
			return true;
		}
		element.replaceChildren(reassembledFragment(element, Array.from(element.childNodes)));
		return false;
	});
}

export function chromeRevertPipeline(root: Element): void {
	const savedChildren = new Map<Element, Array<Node>>();
	walkTranslatable(root, (element) => {
		const children = Array.from(element.childNodes);
		if (children.some(isNonBlankText)) {
			savedChildren.set(element, children);
		}
		return true;
	});
	chromeLegacyFontPipeline(root);
	for (const [element, children] of savedChildren) {
		element.replaceChildren(...children.map((child) => child.cloneNode(true)));
	}
}

export function firefoxMergePipeline(root: Element): void {
	walkTranslatable(root, (element) => {
		const children = Array.from(element.childNodes);
		if (children.length === 0 || !children.some(isNonBlankText)) {
			return true;
		}
		while (element.firstChild) {
			element.removeChild(element.firstChild);
		}
		for (const child of children) {
			element.appendChild(
				child.nodeType === TEXT_NODE ? translatedTextNode(element.ownerDocument, child) : child.cloneNode(true),
			);
		}
		return false;
	});
}

export const TranslatorPipelines = {
	chromeLegacyFont: chromeLegacyFontPipeline,
	chromeCompound: chromeCompoundPipeline,
	chromeRevert: chromeRevertPipeline,
	firefoxMerge: firefoxMergePipeline,
} as const;

export type TranslatorPipelineName = keyof typeof TranslatorPipelines;

export const TRANSLATOR_PIPELINE_NAMES = Object.keys(TranslatorPipelines) as Array<TranslatorPipelineName>;

export function findTranslationUnsafeTextNodes(root: Element): Array<Text> {
	const unsafe: Array<Text> = [];
	const visit = (element: Element): void => {
		const children = Array.from(element.childNodes);
		for (const child of children) {
			if (isElement(child)) {
				visit(child);
			} else if (isNonBlankText(child) && children.length > 1) {
				unsafe.push(child as Text);
			}
		}
	};
	visit(root);
	return unsafe;
}
