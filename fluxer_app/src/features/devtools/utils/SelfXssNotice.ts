// SPDX-License-Identifier: AGPL-3.0-or-later

import {getActiveInstanceProductName} from '@app/features/app/state/ActiveInstanceProductName';
import {DevtoolsDockProbe} from '@app/features/devtools/utils/DevtoolsDockProbe';
import {i18n, type MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';

interface ConsoleLike {
	log: (...data: Array<unknown>) => void;
}

const STOP_DESCRIPTOR = msg({
	message: 'Stop.',
	comment: 'Large console warning headline shown when DevTools opens. Keep it direct and serious.',
});
const DEVELOPER_TOOL_DESCRIPTOR = msg({
	message: 'This browser console is a developer tool.',
	comment: 'Console warning line explaining that the browser console is meant for developers.',
});
const SCAM_WARNING_DESCRIPTOR = msg({
	message: 'If someone told you to paste something here, it is a scam that can hand them your {productName} account.',
	comment: 'Console warning about self-XSS scams. productName is the app name. Keep the warning plain and serious.',
});
const ONLY_RUN_CODE_DESCRIPTOR = msg({
	message: 'Only run code here if you understand exactly what it does.',
	comment: 'Console warning line advising users not to run unknown code.',
});
const SELF_XSS_NOTICE_REPEAT_COUNT = 5;
const SELF_XSS_NOTICE_REPEAT_DELAY_MS = 1000;
const STOP_BANNER_STYLE =
	'color: #36f1cd; font-size: 64px; font-weight: 900; letter-spacing: 0.08em; text-shadow: 0 0 24px rgba(54, 241, 205, 0.45);';
const SUPPORTING_COPY_STYLE = 'font-size: 16px; font-weight: 500;';
const ALERT_COPY_STYLE = 'font-size: 18px; font-weight: 800; color: #ff5c7a;';
const CAUTION_COPY_STYLE = 'font-size: 16px; font-style: italic;';

let selfXssNoticeInstalled = false;

function formatFallbackMessage(message: string, values?: Record<string, string>): string {
	let formatted = message;
	for (const [key, value] of Object.entries(values ?? {})) {
		formatted = formatted.replaceAll(`{${key}}`, value);
	}
	return formatted;
}

function getSelfXssMessage(descriptor: MessageDescriptor, values?: Record<string, string>): string {
	if (i18n.locale) {
		return i18n._(descriptor, values);
	}
	return formatFallbackMessage(descriptor.message ?? descriptor.id, values);
}

function printSelfXssNotice(consoleApi: ConsoleLike = console): void {
	consoleApi.log(`%c${getSelfXssMessage(STOP_DESCRIPTOR)}`, STOP_BANNER_STYLE);
	consoleApi.log(`%c${getSelfXssMessage(DEVELOPER_TOOL_DESCRIPTOR)}`, SUPPORTING_COPY_STYLE);
	consoleApi.log(
		`%c${getSelfXssMessage(SCAM_WARNING_DESCRIPTOR, {productName: getActiveInstanceProductName()})}`,
		ALERT_COPY_STYLE,
	);
	consoleApi.log(`%c${getSelfXssMessage(ONLY_RUN_CODE_DESCRIPTOR)}`, CAUTION_COPY_STYLE);
}

function queueSelfXssNoticeBurst(consoleApi: ConsoleLike = console): () => void {
	const timeoutIds: Array<NodeJS.Timeout> = [];
	for (let repeatIndex = 0; repeatIndex < SELF_XSS_NOTICE_REPEAT_COUNT; repeatIndex++) {
		if (repeatIndex === 0) {
			printSelfXssNotice(consoleApi);
			continue;
		}
		const timeoutId = setTimeout(() => {
			printSelfXssNotice(consoleApi);
		}, repeatIndex * SELF_XSS_NOTICE_REPEAT_DELAY_MS);
		timeoutIds.push(timeoutId);
	}
	return () => {
		for (const timeoutId of timeoutIds) {
			clearTimeout(timeoutId);
		}
	};
}

export function installSelfXssNotice(): () => void {
	if (selfXssNoticeInstalled || import.meta.env.DEV || typeof window === 'undefined') {
		return () => {};
	}
	selfXssNoticeInstalled = true;
	const dockProbe = new DevtoolsDockProbe();
	let hasShownNotice = false;
	let cancelBurst: () => void = () => {};
	let unsubscribe: () => void = () => {};
	const stopDockProbe = (): void => {
		unsubscribe();
		dockProbe.stop();
	};
	unsubscribe = dockProbe.subscribe((state) => {
		if (!state.open || hasShownNotice) {
			return;
		}
		hasShownNotice = true;
		cancelBurst = queueSelfXssNoticeBurst();
		stopDockProbe();
	});
	dockProbe.start();
	return () => {
		cancelBurst();
		stopDockProbe();
		selfXssNoticeInstalled = false;
	};
}
