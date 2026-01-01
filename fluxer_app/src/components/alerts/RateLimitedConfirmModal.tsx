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

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {ConfirmModal} from '~/components/modals/ConfirmModal';

interface RateLimitedConfirmModalProps {
	title: string;
	retryAfter?: number;
	onRetry?: () => void;
}

export const RateLimitedConfirmModal = observer(({title, retryAfter, onRetry}: RateLimitedConfirmModalProps) => {
	const {t} = useLingui();
	const hasRetryAfter = retryAfter != null;

	const formatRateLimitTime = React.useCallback(
		(totalSeconds: number): string => {
			if (totalSeconds < 60) {
				return totalSeconds === 1 ? t`${totalSeconds} second` : t`${totalSeconds} seconds`;
			}

			const minutes = Math.floor(totalSeconds / 60);
			const seconds = totalSeconds % 60;

			if (seconds === 0) {
				return minutes === 1 ? t`${minutes} minute` : t`${minutes} minutes`;
			}

			if (minutes === 1 && seconds === 1) {
				return t`1 minute and 1 second`;
			}

			if (minutes === 1) {
				return t`1 minute and ${seconds} seconds`;
			}

			if (seconds === 1) {
				return t`${minutes} minutes and 1 second`;
			}

			return t`${minutes} minutes and ${seconds} seconds`;
		},
		[t],
	);

	return (
		<ConfirmModal
			title={title}
			description={
				hasRetryAfter
					? t`You're being rate limited. Please wait ${formatRateLimitTime(retryAfter)} before trying again.`
					: t`The problem with being faster than light is that you can only live in darkness. Take a breather and try again.`
			}
			secondaryText={hasRetryAfter ? t`Retry` : t`Gotcha`}
			onSecondary={onRetry}
		/>
	);
});
