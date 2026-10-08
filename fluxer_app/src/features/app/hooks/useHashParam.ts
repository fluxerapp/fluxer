// SPDX-License-Identifier: AGPL-3.0-or-later

import {useEffect, useState} from 'react';

export function readHashParam(hash: string, paramName: string): string | null {
	const prefix = `#${paramName}=`;
	return hash.startsWith(prefix) ? hash.substring(prefix.length) : null;
}

export function useHashParam(paramName: string): string | null {
	const [value, setValue] = useState<string | null>(() => readHashParam(window.location.hash, paramName));
	useEffect(() => {
		const handleHashChange = () => {
			setValue(readHashParam(window.location.hash, paramName));
		};
		window.addEventListener('hashchange', handleHashChange);
		return () => window.removeEventListener('hashchange', handleHashChange);
	}, [paramName]);
	return value;
}
