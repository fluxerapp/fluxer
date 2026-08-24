// SPDX-License-Identifier: AGPL-3.0-or-later

const isElementNode = (node: Node): boolean => {
	const view = node.ownerDocument?.defaultView;
	if (view == null) return false;
	return node instanceof view.Element;
};

export const shouldSearchResultRowJump = (target: Node | null, container: Node | null): boolean => {
	const selection = window?.getSelection();
	if (selection != null && !selection.isCollapsed) return false;
	if (target == null || container == null) return true;
	let node: Node | null = target;
	while (node != null && isElementNode(node) && node !== container) {
		const {tagName} = node as Element;
		if (tagName === 'A' || tagName === 'BUTTON') return false;
		if (tagName === 'IMG' && (node as Element).getAttribute('aria-hidden') !== 'true') return false;
		node = node.parentNode;
	}
	return true;
};
