// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	isInstanceNoticeLive,
	nextInstanceNoticeChangeAt,
} from '@app/features/app/components/layout/app_layout/nagbars/InstanceNoticeSchedule';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import AppStorage from '@app/features/platform/state/PersistentStorage';
import {Logger} from '@app/features/platform/utils/AppLogger';
import type {InstanceNotice} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import {makeAutoObservable} from 'mobx';
import {useEffect, useState} from 'react';

const DISMISS_KEY_PREFIX = 'fluxer_instance_notice_dismissed:';
const LONGEST_TIMER_MS = 24 * 60 * 60 * 1000;
const NOTICE_REFRESH_INTERVAL_MS = 5 * 60 * 1000;

const logger = new Logger('InstanceNotice');
let lastNoticeRefreshAt = Date.now();

function refreshInstanceNoticeIfStale(now: number = Date.now()): void {
	if (now - lastNoticeRefreshAt < NOTICE_REFRESH_INTERVAL_MS) return;
	if (document.visibilityState !== 'visible') return;
	if (RuntimeConfig.getSnapshotOrNull()?.features.self_hosted !== true) return;
	lastNoticeRefreshAt = now;
	RuntimeConfig.revalidateDiscovery().catch((error: unknown) => {
		logger.warn('Instance notice refresh failed', error);
	});
}

export function useInstanceNoticeRefresh(): void {
	useEffect(() => {
		const refresh = () => refreshInstanceNoticeIfStale();
		document.addEventListener('visibilitychange', refresh);
		window.addEventListener('focus', refresh);
		const timer = window.setInterval(refresh, NOTICE_REFRESH_INTERVAL_MS);
		return () => {
			document.removeEventListener('visibilitychange', refresh);
			window.removeEventListener('focus', refresh);
			window.clearInterval(timer);
		};
	}, []);
}

class InstanceNoticeDismissals {
	version = 0;

	constructor() {
		makeAutoObservable(this);
	}

	isDismissed(id: string): boolean {
		void this.version;
		return AppStorage.getItem(`${DISMISS_KEY_PREFIX}${id}`) === '1';
	}

	dismiss(id: string): void {
		AppStorage.setItem(`${DISMISS_KEY_PREFIX}${id}`, '1');
		this.version += 1;
	}
}

export const instanceNoticeDismissals = new InstanceNoticeDismissals();

export function useVisibleInstanceNotice(): InstanceNotice | null {
	const notice = RuntimeConfig.instanceNotice;
	const [tick, setTick] = useState(0);
	useEffect(() => {
		if (!notice) return;
		const nextChangeAt = nextInstanceNoticeChangeAt(notice, Date.now());
		if (nextChangeAt === null) return;
		const timer = window.setTimeout(
			() => setTick((value) => value + 1),
			Math.min(nextChangeAt - Date.now(), LONGEST_TIMER_MS),
		);
		return () => window.clearTimeout(timer);
	}, [notice, tick]);
	if (!notice || !isInstanceNoticeLive(notice, Date.now()) || instanceNoticeDismissals.isDismissed(notice.id)) {
		return null;
	}
	return notice;
}
