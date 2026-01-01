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

import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {ConfirmModal} from '~/components/modals/ConfirmModal';

interface SlowmodeRateLimitedModalProps {
	retryAfter: number;
}

export const SlowmodeRateLimitedModal = observer(({retryAfter}: SlowmodeRateLimitedModalProps) => {
	const {t} = useLingui();

	const formatTime = (seconds: number): string => {
		if (seconds < 60) {
			return seconds === 1 ? t`${seconds} second` : t`${seconds} seconds`;
		}

		const minutes = Math.floor(seconds / 60);
		const remainingSeconds = seconds % 60;

		if (remainingSeconds === 0) {
			return minutes === 1 ? t`${minutes} minute` : t`${minutes} minutes`;
		}

		if (minutes === 1 && remainingSeconds === 1) {
			return t`1 minute and 1 second`;
		}

		if (minutes === 1) {
			return t`1 minute and ${remainingSeconds} seconds`;
		}

		if (remainingSeconds === 1) {
			return t`${minutes} minutes and 1 second`;
		}

		return t`${minutes} minutes and ${remainingSeconds} seconds`;
	};

	return (
		<ConfirmModal
			title={t`Slowmode Active`}
			description={t(
				msg`This channel has slowmode enabled. You need to wait ${formatTime(retryAfter)} before sending another message.`,
			)}
			primaryText={t`Okay`}
			onPrimary={() => {}}
		/>
	);
});
