// SPDX-License-Identifier: AGPL-3.0-or-later

function isTextEntryInputType(inputType: string): boolean {
	switch (inputType) {
		case 'email':
		case 'number':
		case 'password':
		case 'search':
		case 'tel':
		case 'text':
		case 'url':
			return true;
		default:
			return false;
	}
}

export const TextEntryElementPolicy = Object.freeze({
	isTextEntry(element: HTMLElement): boolean {
		if (element.isContentEditable) {
			return true;
		}
		if (element.tagName === 'TEXTAREA') {
			return true;
		}
		if (element.tagName === 'INPUT') {
			return isTextEntryInputType((element as HTMLInputElement).type.toLowerCase());
		}
		return element.getAttribute('role') === 'textbox';
	},
});
