// SPDX-License-Identifier: AGPL-3.0-or-later

const EMPTY_INSIDE_ELEMENTS: Array<Element> = [];

export function getPopoutFocusManagerInsideElements(
	referenceElement?: HTMLElement | null,
	returnFocusElement?: HTMLElement | null,
): Array<Element> {
	const insideElements: Array<Element> = [];
	const addElement = (element: Element | null | undefined): void => {
		if (!element?.isConnected || insideElements.includes(element)) {
			return;
		}
		insideElements.push(element);
	};

	addElement(referenceElement);
	addElement(returnFocusElement);
	addElement(typeof document === 'undefined' ? null : document.querySelector('[data-native-titlebar]'));

	return insideElements.length ? insideElements : EMPTY_INSIDE_ELEMENTS;
}
