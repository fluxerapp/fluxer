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
import {PushPinIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {ChannelPinsBottomSheet} from '~/components/bottomsheets/ChannelPinsBottomSheet';
import {ChannelPinsPopout} from '~/components/popouts/ChannelPinsPopout';
import {Popout} from '~/components/uikit/Popout/Popout';
import {usePopout} from '~/hooks/usePopout';
import type {ChannelRecord} from '~/records/ChannelRecord';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import ReadStateStore from '~/stores/ReadStateStore';
import styles from '../ChannelHeader.module.css';
import {ChannelHeaderIcon} from './ChannelHeaderIcon';

export const ChannelPinsButton = observer(({channel}: {channel: ChannelRecord}) => {
	const {t} = useLingui();
	const {isOpen, openProps} = usePopout('channel-pins');
	const isMobile = MobileLayoutStore.isMobileLayout();
	const [isBottomSheetOpen, setIsBottomSheetOpen] = React.useState(false);
	const hasUnreadPins = ReadStateStore.hasUnreadPins(channel.id);

	const handleClick = React.useCallback(() => {
		if (isMobile) {
			setIsBottomSheetOpen(true);
		}
	}, [isMobile]);

	const indicator = hasUnreadPins ? <div className={styles.unreadPinIndicator} /> : null;

	if (isMobile) {
		return (
			<>
				<div className={styles.iconButtonWrapper}>
					<ChannelHeaderIcon
						icon={PushPinIcon}
						label={t`Pinned Messages`}
						isSelected={isBottomSheetOpen}
						onClick={handleClick}
						keybindAction="toggle_pins_popout"
					/>
					{indicator}
				</div>
				<ChannelPinsBottomSheet
					isOpen={isBottomSheetOpen}
					onClose={() => setIsBottomSheetOpen(false)}
					channel={channel}
				/>
			</>
		);
	}

	return (
		<Popout
			{...openProps}
			render={() => <ChannelPinsPopout channel={channel} />}
			position="bottom-end"
			subscribeTo="CHANNEL_PINS_OPEN"
		>
			<div className={styles.iconButtonWrapper}>
				<ChannelHeaderIcon
					icon={PushPinIcon}
					label={t`Pinned Messages`}
					isSelected={isOpen}
					keybindAction="toggle_pins_popout"
				/>
				{indicator}
			</div>
		</Popout>
	);
});
