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
import {MagnifyingGlassIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import type {ChannelRecord} from '~/records/ChannelRecord';
import * as ChannelUtils from '~/utils/ChannelUtils';
import {AutocompleteOption} from './AutocompleteOption';
import styles from './MessageSearchBar.module.css';

interface ChannelsSectionProps {
	options: Array<ChannelRecord>;
	selectedIndex: number;
	hoverIndex: number;
	onSelect: (channel: ChannelRecord) => void;
	onMouseEnter: (index: number) => void;
	onMouseLeave?: () => void;
	listboxId: string;
}

export const ChannelsSection: React.FC<ChannelsSectionProps> = observer(
	({options, selectedIndex, hoverIndex, onSelect, onMouseEnter, onMouseLeave, listboxId}) => {
		const {t} = useLingui();

		if (options.length === 0) return null;

		return (
			<div className={styles.popoutSection}>
				<div className={styles.popoutSectionHeader}>
					<span className={`${styles.flex} ${styles.itemsCenter} ${styles.gap2}`}>
						<MagnifyingGlassIcon weight="regular" size={14} />
						{t`Channels`}
					</span>
				</div>
				{options.map((channelOption: ChannelRecord, index) => (
					<AutocompleteOption
						key={channelOption.id}
						index={index}
						isSelected={index === selectedIndex}
						isHovered={index === hoverIndex}
						onSelect={() => onSelect(channelOption)}
						onMouseEnter={() => onMouseEnter(index)}
						onMouseLeave={onMouseLeave}
						listboxId={listboxId}
					>
						<div className={styles.optionLabel}>
							<div className={styles.optionContent}>
								<div className={styles.channelRow}>
									{ChannelUtils.getIcon(channelOption, {className: styles.channelIcon})}
									<span className={styles.channelName}>{channelOption.name || 'Unnamed Channel'}</span>
								</div>
							</div>
						</div>
					</AutocompleteOption>
				))}
			</div>
		);
	},
);
