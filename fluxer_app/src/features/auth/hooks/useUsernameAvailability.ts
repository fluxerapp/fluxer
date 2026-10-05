// SPDX-License-Identifier: AGPL-3.0-or-later

import * as AuthenticationCommands from '@app/features/auth/commands/AuthenticationCommands';
import {useEffect, useState} from 'react';

export type UsernameAvailability = 'idle' | 'checking' | 'available' | 'taken';

const CHECK_DELAY_MS = 350;

export function useUsernameAvailability(username: string, enabled: boolean): UsernameAvailability {
	const [result, setResult] = useState<{username: string; availability: UsernameAvailability}>({
		username: '',
		availability: 'idle',
	});
	useEffect(() => {
		if (!enabled || username.length === 0) {
			return;
		}
		const controller = new AbortController();
		const timer = window.setTimeout(() => {
			setResult({username, availability: 'checking'});
			AuthenticationCommands.checkUsernameAvailability(username, controller.signal)
				.then((available) => {
					if (controller.signal.aborted) return;
					setResult({username, availability: available ? 'available' : 'taken'});
				})
				.catch(() => {
					if (controller.signal.aborted) return;
					setResult({username, availability: 'idle'});
				});
		}, CHECK_DELAY_MS);
		return () => {
			window.clearTimeout(timer);
			controller.abort();
		};
	}, [enabled, username]);
	if (!enabled || username.length === 0 || result.username !== username) {
		return 'idle';
	}
	return result.availability;
}
