/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {useEffect, useState} from 'react';

export function useHashParam(paramName: string): string | null {
	const [value, setValue] = useState<string | null>(() => {
		const hash = window.location.hash;
		if (hash?.startsWith(`#${paramName}=`)) {
			return hash.substring(`#${paramName}=`.length);
		}
		return null;
	});

	useEffect(() => {
		const handleHashChange = () => {
			const hash = window.location.hash;
			if (hash?.startsWith(`#${paramName}=`)) {
				setValue(hash.substring(`#${paramName}=`.length));
			} else {
				setValue(null);
			}
		};

		window.addEventListener('hashchange', handleHashChange);
		return () => window.removeEventListener('hashchange', handleHashChange);
	}, [paramName]);

	return value;
}
