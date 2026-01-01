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

import React from 'react';

export const useInviteCountdown = (
	expiresAt: string | null | undefined,
): {countdown: string | null; isMonospace: boolean} => {
	const [countdown, setCountdown] = React.useState<string | null>(null);
	const [isMonospace, setIsMonospace] = React.useState(false);

	React.useEffect(() => {
		if (!expiresAt) {
			setCountdown(null);
			setIsMonospace(false);
			return;
		}

		const updateTime = () => {
			const expiresAtTime = new Date(expiresAt).getTime();
			const now = Date.now();
			const remaining = expiresAtTime - now;

			if (remaining <= 0) {
				setCountdown('Expired');
				setIsMonospace(false);
				return;
			}

			const days = Math.floor(remaining / (1000 * 60 * 60 * 24));
			const hours = Math.floor((remaining % (1000 * 60 * 60 * 24)) / (1000 * 60 * 60));
			const minutes = Math.floor((remaining % (1000 * 60 * 60)) / (1000 * 60));
			const seconds = Math.floor((remaining % (1000 * 60)) / 1000);

			const parts: Array<string> = [];
			if (days > 0) {
				parts.push(String(days).padStart(2, '0'));
			}
			parts.push(String(hours).padStart(2, '0'));
			parts.push(String(minutes).padStart(2, '0'));
			parts.push(String(seconds).padStart(2, '0'));

			setCountdown(parts.join(':'));
			setIsMonospace(true);
		};

		updateTime();
		const interval = setInterval(updateTime, 1000);

		return () => clearInterval(interval);
	}, [expiresAt]);

	return {countdown, isMonospace};
};
