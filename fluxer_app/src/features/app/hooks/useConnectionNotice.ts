// SPDX-License-Identifier: AGPL-3.0-or-later

import {canSwitchAccountFromStalledConnection} from '@app/features/app/ConnectionRecovery';
import {isClientBooting, isClientReconnecting} from '@app/features/app/state/ClientReadiness';
import Nagbar from '@app/features/ui/state/Nagbar';
import StatusPage from '@app/features/user/state/StatusPage';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useEffect, useState} from 'react';

const BOOT_NOTICE_DELAY_MS = 3_000;
const STATUS_PAGE_URL = RuntimeConfig.statusPageUrl;

export const ConnectionNoticeTone = Object.freeze({
	NEUTRAL: 'neutral',
	MAINTENANCE: 'maintenance',
} as const);

export type ConnectionNoticeTone = (typeof ConnectionNoticeTone)[keyof typeof ConnectionNoticeTone];

export interface ConnectionNoticeAction {
	readonly label: string;
	readonly url: string;
}

export interface ConnectionNotice {
	readonly tone: ConnectionNoticeTone;
	readonly message: string;
	readonly action: ConnectionNoticeAction | null;
	readonly showSwitchAccount: boolean;
}

const CONNECTION_ISSUES_DESCRIPTOR = msg({
	message: 'Connection issues?',
	comment:
		'Banner shown while the app is still loading and the connection is taking unusually long. Phrased as a question offering help.',
});
const CONNECTION_LOST_DESCRIPTOR = msg({
	message: 'Connection lost. Reconnecting…',
	comment:
		'Banner shown when the app loses its gateway connection after having loaded. The app stays usable, so keep the tone calm.',
});
const VIEW_STATUS_PAGE_DESCRIPTOR = msg({
	message: 'View status page',
	comment: 'Button on the connection banner. Opens the external service status page.',
});
const VIEW_INCIDENT_DETAILS_DESCRIPTOR = msg({
	message: 'View incident details',
	comment: 'Button on the connection banner when an incident is ongoing. Opens that incident on the status page.',
});
const VIEW_MAINTENANCE_DETAILS_DESCRIPTOR = msg({
	message: 'View maintenance details',
	comment: 'Button on the connection banner when maintenance is ongoing. Opens that maintenance on the status page.',
});

export interface ConnectionNoticeShape {
	readonly tone: ConnectionNoticeTone;
	readonly hasActions: boolean;
}

export function resolveConnectionNoticeShape(): ConnectionNoticeShape {
	if (isClientReconnecting()) {
		return {tone: ConnectionNoticeTone.NEUTRAL, hasActions: false};
	}
	if (StatusPage.scheduledMaintenance != null) {
		return {tone: ConnectionNoticeTone.MAINTENANCE, hasActions: true};
	}
	return {tone: ConnectionNoticeTone.NEUTRAL, hasActions: true};
}

export function useConnectionNotice(): ConnectionNotice | null {
	const {i18n} = useLingui();
	const booting = isClientBooting();
	const reconnecting = isClientReconnecting();
	const forced = Nagbar.forceConnectionNotice;
	const showSwitchAccount = canSwitchAccountFromStalledConnection();
	const [bootStalled, setBootStalled] = useState(false);
	useEffect(() => {
		if (!booting) {
			setBootStalled(false);
			return;
		}
		const timer = window.setTimeout(() => {
			setBootStalled(true);
			void StatusPage.checkIncidents();
		}, BOOT_NOTICE_DELAY_MS);
		return () => window.clearTimeout(timer);
	}, [booting]);
	useEffect(() => {
		if (!reconnecting) {
			return;
		}
		StatusPage.refreshForConnectionIssue();
	}, [reconnecting]);
	if (Nagbar.forceHideConnectionNotice) {
		return null;
	}
	let connectionUnavailable = reconnecting;
	if (booting) {
		connectionUnavailable = bootStalled;
	}
	if (!forced && !connectionUnavailable) {
		return null;
	}
	if (reconnecting) {
		return {
			tone: ConnectionNoticeTone.NEUTRAL,
			message: i18n._(CONNECTION_LOST_DESCRIPTOR),
			action: null,
			showSwitchAccount: false,
		};
	}
	const maintenance = StatusPage.scheduledMaintenance;
	if (maintenance != null) {
		return {
			tone: ConnectionNoticeTone.MAINTENANCE,
			message: maintenance.name,
			action: {label: i18n._(VIEW_MAINTENANCE_DETAILS_DESCRIPTOR), url: maintenance.url},
			showSwitchAccount,
		};
	}
	const incident = StatusPage.incident;
	if (incident != null) {
		return {
			tone: ConnectionNoticeTone.NEUTRAL,
			message: incident.name,
			action: {label: i18n._(VIEW_INCIDENT_DETAILS_DESCRIPTOR), url: incident.url},
			showSwitchAccount,
		};
	}
	return {
		tone: ConnectionNoticeTone.NEUTRAL,
		message: i18n._(CONNECTION_ISSUES_DESCRIPTOR),
		action: STATUS_PAGE_URL ? {label: i18n._(VIEW_STATUS_PAGE_DESCRIPTOR), url: STATUS_PAGE_URL} : null,
		showSwitchAccount,
	};
}
