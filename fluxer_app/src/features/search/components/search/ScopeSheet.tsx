// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ScopeValueOption} from '@app/features/channel/components/SearchScopeOptions';
import styles from '@app/features/search/components/search/ScopeSheet.module.css';
import type {MessageSearchScope} from '@app/features/search/utils/SearchUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Scroller} from '@app/features/ui/components/Scroller';
import {msg} from '@lingui/core/macro';
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

const SEARCH_IN_DESCRIPTOR = msg({
	message: 'Search in',
	comment: 'Button or menu action label in the search scope sheet. Keep it concise.',
});
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
	const {i18n} = useLingui();
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
			title={i18n._(SEARCH_IN_DESCRIPTOR)}
			disablePadding
			data-flx="search.search.scope-sheet.bottom-sheet"
		>
			<div className={styles.container} data-flx="search.search.scope-sheet.container">
				<Scroller
					key="scope-sheet-scroller"
					className={styles.scroller}
					fade={false}
					data-flx="search.search.scope-sheet.scroller"
				>
					<div className={styles.optionsContainer} data-flx="search.search.scope-sheet.options-container">
						{scopeOptions.map((option) => {
							const isSelected = selectedScope === option.value;
							const Icon = SCOPE_ICON_COMPONENTS[option.value] ?? HashIcon;
							return (
								<button
									key={option.value}
									type="button"
									aria-pressed={isSelected}
									className={clsx(styles.option, isSelected && styles.optionSelected)}
									onClick={() => handleSelect(option.value)}
									data-flx="search.search.scope-sheet.option.select.button"
								>
									<div className={styles.optionLeft} data-flx="search.search.scope-sheet.option-left">
										<Icon
											size={remFromPx(22)}
											className={clsx(styles.optionIcon, isSelected && styles.optionIconSelected)}
											weight="regular"
											data-flx="search.search.scope-sheet.option-icon"
										/>
										<div className={styles.optionText} data-flx="search.search.scope-sheet.option-text">
											<span
												className={clsx(styles.optionLabel, isSelected && styles.optionLabelSelected)}
												data-flx="search.search.scope-sheet.option-label"
											>
												{option.label}
											</span>
											{option.description && (
												<span
													className={styles.optionDescription}
													data-flx="search.search.scope-sheet.option-description"
												>
													{option.description}
												</span>
											)}
										</div>
									</div>
									{isSelected && (
										<CheckIcon
											size={remFromPx(20)}
											className={styles.checkIcon}
											weight="bold"
											data-flx="search.search.scope-sheet.check-icon"
										/>
									)}
								</button>
							);
						})}
					</div>
				</Scroller>
			</div>
		</BottomSheet>
	);
};
