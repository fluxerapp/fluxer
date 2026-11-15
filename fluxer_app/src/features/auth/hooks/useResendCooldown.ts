// SPDX-License-Identifier: AGPL-3.0-or-later

import {secondsUntil} from '@app/features/auth/flow/identifier_first/IdentifierFirstState';
import {useEffect, useState} from 'react';

export function useResendCooldown(resendAvailableAt: number | null): number {
	const [now, setNow] = useState(() => Date.now());
	useEffect(() => {
		setNow(Date.now());
		if (resendAvailableAt === null || resendAvailableAt <= Date.now()) return;
		const id = window.setInterval(() => {
			const current = Date.now();
			setNow(current);
			if (current >= resendAvailableAt) window.clearInterval(id);
		}, 1000);
		return () => window.clearInterval(id);
	}, [resendAvailableAt]);
	return resendAvailableAt === null ? 0 : secondsUntil(resendAvailableAt, now);
}
