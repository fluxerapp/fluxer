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
import {ChannelPinsContent} from '~/components/shared/ChannelPinsContent';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import type {ChannelRecord} from '~/records/ChannelRecord';

export const ChannelPinsBottomSheet = observer(
	({isOpen, onClose, channel}: {isOpen: boolean; onClose: () => void; channel: ChannelRecord}) => {
		const {t} = useLingui();
		return (
			<BottomSheet isOpen={isOpen} onClose={onClose} title={t`Pinned Messages`} snapPoints={[0, 1]} initialSnap={1}>
				<ChannelPinsContent channel={channel} onJump={onClose} />
			</BottomSheet>
		);
	},
);
