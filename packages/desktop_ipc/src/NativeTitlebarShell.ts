// SPDX-License-Identifier: AGPL-3.0-or-later

export const NATIVE_TITLEBAR_CONTROL_ATTR = 'data-window-control';

export type NativeTitlebarControlName = 'minimize' | 'maximize' | 'close';
export type NativeTitlebarLayout = 'macos' | 'controls';

export const NATIVE_TITLEBAR_CLASS = {
	root: 'fluxer-native-titlebar',
	left: 'fluxer-native-titlebar__left',
	wordmark: 'fluxer-native-titlebar__wordmark',
	spacer: 'fluxer-native-titlebar__spacer',
	controls: 'fluxer-native-titlebar__controls',
	control: 'fluxer-native-titlebar__control',
	controlClose: 'fluxer-native-titlebar__control--close',
} as const;

export const NATIVE_TITLEBAR_WORDMARK_VIEWBOX = '0 0 1374 366';

export const NATIVE_TITLEBAR_WORDMARK_PATHS: ReadonlyArray<string> = [
	'M0 358.4V20.48h82.944V358.4H0Zm41.472-120.32v-67.584h172.032v67.584H41.472Zm0-148.48V20.48h189.44V89.6H41.472ZM258.951 358.4V0h82.944v358.4h-82.944ZM462.92 365.568c-29.35 0-51.2-10.24-65.536-30.72-13.995-20.48-20.992-51.883-20.992-94.208V88.064h82.948v151.552c0 19.797 2.73 33.621 8.19 41.472 5.8 7.851 13.99 11.776 24.57 11.776 7.51 0 14.34-1.877 20.48-5.632 6.49-4.096 12.12-10.069 16.9-17.92 4.78-8.192 8.36-18.603 10.75-31.232 2.73-12.629 4.1-27.819 4.1-45.568V88.064h82.94V358.4h-70.65V252.928h-3.59c-2.05 26.283-6.65 47.787-13.82 64.512-7.17 16.384-17.07 28.501-29.7 36.352-12.63 7.851-28.16 11.776-46.59 11.776ZM647.569 358.4l84.99-135.68-83.97-134.656h96.26l39.42 87.552h2.56l38.4-87.552h95.23l-81.4 134.656 82.43 135.68h-97.79l-37.38-86.016h-2.05l-41.47 86.016h-95.23Z',
	'M1045.96 365.568c-25.6 0-47.101-3.584-64.511-10.752-17.06-7.509-30.89-17.579-41.47-30.208-10.58-12.971-18.26-27.648-23.04-44.032-4.44-16.384-6.66-33.621-6.66-51.712 0-19.456 2.39-38.059 7.17-55.808 5.12-17.749 12.8-33.451 23.04-47.104 10.58-13.995 24.07-24.917 40.45-32.768 16.73-8.192 36.691-12.288 59.901-12.288s43.01 4.096 59.4 12.288c16.72 7.851 30.03 18.944 39.93 33.28 9.9 14.336 16.22 30.891 18.95 49.664 3.07 18.773 2.73 39.083-1.03 60.928l-196.091 3.072v-45.056l132.601-2.56-10.75 26.112c2.05-15.701 1.71-28.843-1.02-39.424-2.39-10.923-7-19.115-13.83-24.576-6.82-5.803-16.21-8.704-28.16-8.704-12.63 0-22.69 3.243-30.2 9.728-7.51 6.485-12.801 15.701-15.881 27.648-3.07 11.605-4.6 25.429-4.6 41.472 0 27.648 4.6 47.787 13.821 60.416 9.22 12.629 23.38 18.944 42.5 18.944 8.19 0 15.01-1.024 20.48-3.072 5.46-2.048 9.89-4.949 13.31-8.704 3.41-4.096 5.8-8.875 7.17-14.336 1.36-5.803 1.87-12.288 1.53-19.456l75.78 4.096c1.02 11.264-.17 22.869-3.59 34.816-3.07 11.947-9.04 23.04-17.92 33.28-8.87 10.24-21.33 18.603-37.37 25.088-15.7 6.485-35.67 9.728-59.91 9.728ZM1193.45 358.4V88.0639h71.68V195.584h4.1c2.05-28.672 5.97-51.2 11.77-67.584 6.15-16.725 13.66-28.5011 22.53-35.3281 9.22-7.168 19.46-10.752 30.72-10.752 6.15 0 12.46.853 18.95 2.56 6.82 1.707 13.48 4.437 19.96 8.192l-4.09 92.1601c-7.51-4.437-14.85-7.68-22.02-9.728-7.17-2.389-13.99-3.584-20.48-3.584-10.92 0-20.14 3.072-27.65 9.216-7.51 6.144-13.31 15.189-17.4 27.136-3.76 11.947-5.64 26.453-5.64 43.52V358.4h-82.43Z',
];

export const NATIVE_TITLEBAR_CONTROL_ICON_PATH = {
	minimize: 'M228,128a12,12,0,0,1-12,12H40a12,12,0,0,1,0-24H216A12,12,0,0,1,228,128Z',
	maximize:
		'M208,28H48A20,20,0,0,0,28,48V208a20,20,0,0,0,20,20H208a20,20,0,0,0,20-20V48A20,20,0,0,0,208,28Zm-4,176H52V52H204Z',
	restore:
		'M180,64H40A12,12,0,0,0,28,76V216a12,12,0,0,0,12,12H180a12,12,0,0,0,12-12V76A12,12,0,0,0,180,64ZM168,204H52V88H168ZM228,40V180a12,12,0,0,1-24,0V52H76a12,12,0,0,1,0-24H216A12,12,0,0,1,228,40Z',
	close:
		'M208.49,191.51a12,12,0,0,1-17,17L128,145,64.49,208.49a12,12,0,0,1-17-17L111,128,47.51,64.49a12,12,0,0,1,17-17L128,111l63.51-63.52a12,12,0,0,1,17,17L145,128Z',
} as const;

export type NativeTitlebarControlIconName = keyof typeof NATIVE_TITLEBAR_CONTROL_ICON_PATH;

export function nativeTitlebarControlIconSvg(icon: NativeTitlebarControlIconName): string {
	return (
		'<svg viewBox="0 0 256 256" fill="currentColor" xmlns="http://www.w3.org/2000/svg">' +
		`<path d="${NATIVE_TITLEBAR_CONTROL_ICON_PATH[icon]}"/></svg>`
	);
}

export function nativeTitlebarWordmarkSvg(className: string = NATIVE_TITLEBAR_CLASS.wordmark): string {
	const paths = NATIVE_TITLEBAR_WORDMARK_PATHS.map((d) => `<path fill="currentColor" d="${d}"/>`).join('');
	return (
		`<svg class="${className}" role="img" aria-label="Fluxer" xmlns="http://www.w3.org/2000/svg" ` +
		`fill="none" viewBox="${NATIVE_TITLEBAR_WORDMARK_VIEWBOX}">${paths}</svg>`
	);
}

interface StartupControlLabels {
	readonly minimize: string;
	readonly maximize: string;
	readonly close: string;
}

const DEFAULT_STARTUP_CONTROL_LABELS: StartupControlLabels = {
	minimize: 'Minimize window',
	maximize: 'Maximize window',
	close: 'Close window',
};

function controlButtonHtml(
	name: NativeTitlebarControlName,
	icon: NativeTitlebarControlIconName,
	label: string,
	extraClass?: string,
): string {
	const className = extraClass ? `${NATIVE_TITLEBAR_CLASS.control} ${extraClass}` : NATIVE_TITLEBAR_CLASS.control;
	return (
		`<button type="button" tabindex="-1" class="${className}" ${NATIVE_TITLEBAR_CONTROL_ATTR}="${name}" ` +
		`aria-label="${label}">${nativeTitlebarControlIconSvg(icon)}</button>`
	);
}

export function nativeTitlebarControlsHtml(labels: StartupControlLabels = DEFAULT_STARTUP_CONTROL_LABELS): string {
	return (
		`<div class="${NATIVE_TITLEBAR_CLASS.controls}">` +
		controlButtonHtml('minimize', 'minimize', labels.minimize) +
		controlButtonHtml('maximize', 'maximize', labels.maximize) +
		controlButtonHtml('close', 'close', labels.close, NATIVE_TITLEBAR_CLASS.controlClose) +
		'</div>'
	);
}

export function buildNativeTitlebarStartupHtml(layout: NativeTitlebarLayout): string {
	const brand = `<div class="${NATIVE_TITLEBAR_CLASS.left}">${nativeTitlebarWordmarkSvg()}</div>`;
	const spacer = `<div class="${NATIVE_TITLEBAR_CLASS.spacer}"></div>`;
	if (layout === 'macos') {
		return `${spacer}${brand}`;
	}
	return `${brand}${spacer}${nativeTitlebarControlsHtml()}`;
}
