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
import type {IconProps} from '@phosphor-icons/react';
import {
	ChatCenteredDotsIcon,
	CheckIcon,
	EnvelopeSimpleIcon,
	GlobeIcon,
	HashIcon,
	UsersIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import type React from 'react';
import type {ScopeValueOption} from '~/components/channel/searchScopeOptions';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import {Scroller} from '~/components/uikit/Scroller';
import type {MessageSearchScope} from '~/utils/SearchUtils';
import styles from './ScopeSheet.module.css';

const SCOPE_ICON_COMPONENTS: Record<MessageSearchScope, React.ComponentType<IconProps>> = {
	current: HashIcon,
	all_dms: EnvelopeSimpleIcon,
	open_dms: ChatCenteredDotsIcon,
	all_guilds: GlobeIcon,
	all: UsersIcon,
	open_dms_and_all_guilds: UsersIcon,
};

interface ScopeSheetProps {
	isOpen: boolean;
	onClose: () => void;
	selectedScope: MessageSearchScope;
	scopeOptions: Array<ScopeValueOption>;
	onScopeChange: (scope: MessageSearchScope) => void;
}

export const ScopeSheet: React.FC<ScopeSheetProps> = ({
	isOpen,
	onClose,
	selectedScope,
	scopeOptions,
	onScopeChange,
}) => {
	const {t} = useLingui();
	const handleSelect = (scope: MessageSearchScope) => {
		onScopeChange(scope);
		onClose();
	};

	return (
		<BottomSheet
			isOpen={isOpen}
			onClose={onClose}
			snapPoints={[0, 1]}
			initialSnap={1}
			title={t`Search in`}
			disablePadding
		>
			<div className={styles.container}>
				<Scroller className={styles.scroller} fade={false}>
					<div className={styles.optionsContainer}>
						{scopeOptions.map((option) => {
							const isSelected = selectedScope === option.value;
							const Icon = SCOPE_ICON_COMPONENTS[option.value] ?? HashIcon;
							return (
								<button
									key={option.value}
									type="button"
									className={clsx(styles.option, isSelected && styles.optionSelected)}
									onClick={() => handleSelect(option.value)}
								>
									<div className={styles.optionLeft}>
										<Icon
											size={22}
											className={clsx(styles.optionIcon, isSelected && styles.optionIconSelected)}
											weight="regular"
										/>
										<div className={styles.optionText}>
											<span className={clsx(styles.optionLabel, isSelected && styles.optionLabelSelected)}>
												{option.label}
											</span>
											{option.description && <span className={styles.optionDescription}>{option.description}</span>}
										</div>
									</div>
									{isSelected && <CheckIcon size={20} className={styles.checkIcon} weight="bold" />}
								</button>
							);
						})}
					</div>
				</Scroller>
			</div>
		</BottomSheet>
	);
};
