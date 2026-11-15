// SPDX-License-Identifier: AGPL-3.0-or-later

import {useEffect, useState} from 'react';

const ANDROID_APP_NAGBAR_STARTS_AT_MS = Date.parse('2026-11-09T12:00:00.000Z');
const LONGEST_TIMER_MS = 24 * 60 * 60 * 1000;

export function useAndroidAppNagbarStarted(): boolean {
	const [now, setNow] = useState(Date.now);
	const started = now >= ANDROID_APP_NAGBAR_STARTS_AT_MS;
	useEffect(() => {
		if (started) return;
		const timer = window.setTimeout(
			() => setNow(Date.now()),
			Math.min(ANDROID_APP_NAGBAR_STARTS_AT_MS - now, LONGEST_TIMER_MS),
		);
		return () => window.clearTimeout(timer);
	}, [started, now]);
	return started;
}
