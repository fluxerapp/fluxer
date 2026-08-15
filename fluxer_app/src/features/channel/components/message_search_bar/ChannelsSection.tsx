// SPDX-License-Identifier: AGPL-3.0-or-later

import {AutocompleteOption} from '@app/features/channel/components/message_search_bar/AutocompleteOption';
import styles from '@app/features/channel/components/message_search_bar/MessageSearchBar.module.css';
import type {Channel} from '@app/features/channel/models/Channel';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const CHANNELS_DESCRIPTOR = msg({
	message: 'Channels',
	comment: 'Section header in the message search popout listing matching channels for the in: filter. Title Case.',
});

interface ChannelsSectionProps {
	options: Array<Channel>;
	selectedIndex: number;
	hoverIndex: number;
	onSelect: (channel: Channel) => void;
	onMouseEnter: (index: number) => void;
	onMouseLeave?: () => void;
	listboxId: string;
}

export const ChannelsSection: React.FC<ChannelsSectionProps> = observer(
	({options, selectedIndex, hoverIndex, onSelect, onMouseEnter, onMouseLeave, listboxId}) => {
		const {i18n} = useLingui();
		if (options.length === 0) return null;
		return (
			<div className={styles.popoutSection} data-flx="channel.message-search-bar.channels-section.popout-section">
				<div
					className={styles.popoutSectionHeader}
					data-flx="channel.message-search-bar.channels-section.popout-section-header"
				>
					<span
						className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}
						data-flx="channel.message-search-bar.channels-section.flex"
					>
						<MagnifyingGlassIcon
							weight="regular"
							size={remFromPx(14)}
							data-flx="channel.message-search-bar.channels-section.magnifying-glass-icon"
						/>
						{i18n._(CHANNELS_DESCRIPTOR)}
					</span>
				</div>
				{options.map((channelOption: Channel, index) => (
					<AutocompleteOption
						key={channelOption.id}
						index={index}
						isSelected={index === selectedIndex}
						isHovered={index === hoverIndex}
						onSelect={() => onSelect(channelOption)}
						onMouseEnter={() => onMouseEnter(index)}
						onMouseLeave={onMouseLeave}
						listboxId={listboxId}
						data-flx="channel.message-search-bar.channels-section.autocomplete-option.select"
					>
						<div className={styles.optionLabel} data-flx="channel.message-search-bar.channels-section.option-label">
							<div
								className={styles.optionContent}
								data-flx="channel.message-search-bar.channels-section.option-content"
							>
								<div className={styles.channelRow} data-flx="channel.message-search-bar.channels-section.channel-row">
									{ChannelUtils.getIcon(channelOption, {className: styles.channelIcon})}
									<span
										className={styles.channelName}
										data-flx="channel.message-search-bar.channels-section.channel-name"
									>
										{channelOption.name || 'Unnamed Channel'}
									</span>
								</div>
							</div>
						</div>
					</AutocompleteOption>
				))}
			</div>
		);
	},
);
