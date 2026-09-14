// SPDX-License-Identifier: AGPL-3.0-or-later

import {PREMIUM_PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
import {shouldShowPremiumFeatures} from '@app/features/premium/utils/PremiumUtils';
import {CharacterCountAnnouncer} from '@app/features/ui/character_counter/CharacterCountAnnouncer';
import styles from '@app/features/ui/character_counter/CharacterCounter.module.css';
import {
	CHARACTERS_LEFT_DESCRIPTOR,
	CHARACTERS_LEFT_GET_TO_WRITE_UP_TO_CHARACTERS_DESCRIPTOR,
	MESSAGE_IS_TOO_LONG_DESCRIPTOR,
} from '@app/features/ui/character_counter/CharacterCountMessages';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

interface CharacterCounterProps {
	currentLength: number;
	maxLength: number;
	canUpgrade: boolean;
	premiumMaxLength: number;
	onUpgradeClick: () => void;
	className?: string;
}

export const CharacterCounter = observer(
	({currentLength, maxLength, canUpgrade, premiumMaxLength, onUpgradeClick, className}: CharacterCounterProps) => {
		const {i18n} = useLingui();
		const remaining = maxLength - currentLength;
		const formattedRemaining = getCachedNumberFormat(i18n.locale).format(remaining);
		const isOverLimit = remaining < 0;
		const isNearingLimit = remaining < 50;
		const showPremiumFeatures = shouldShowPremiumFeatures();
		const needsPremium = canUpgrade && showPremiumFeatures && (isNearingLimit || isOverLimit);
		const tooltipText = needsPremium
			? i18n._(CHARACTERS_LEFT_GET_TO_WRITE_UP_TO_CHARACTERS_DESCRIPTOR, {
					remaining,
					premiumProductName: PREMIUM_PRODUCT_NAME,
					premiumMaxLength,
				})
			: isOverLimit
				? i18n._(MESSAGE_IS_TOO_LONG_DESCRIPTOR)
				: i18n._(CHARACTERS_LEFT_DESCRIPTOR, {remaining});
		const colorClass = isOverLimit || isNearingLimit ? styles.textDanger : styles.textTertiary;
		const counter = needsPremium ? (
			<Tooltip text={tooltipText} data-flx="ui.character-counter.character-counter.tooltip">
				<FocusRing offset={-2} data-flx="ui.character-counter.character-counter.focus-ring">
					<button
						type="button"
						onClick={onUpgradeClick}
						aria-label={tooltipText}
						data-flx="ui.character-counter.character-counter.counter-button.upgrade-click"
						className={clsx(styles.counterButton, colorClass, className)}
					>
						<span aria-hidden="true" data-flx="ui.character-counter.character-counter.span">
							{formattedRemaining}
						</span>
					</button>
				</FocusRing>
			</Tooltip>
		) : (
			<Tooltip text={tooltipText} data-flx="ui.character-counter.character-counter.tooltip--2">
				<span
					aria-hidden="true"
					data-flx="ui.character-counter.character-counter.counter-span"
					className={clsx(styles.counterSpan, colorClass, className)}
				>
					{formattedRemaining}
				</span>
			</Tooltip>
		);
		return (
			<>
				{counter}
				<CharacterCountAnnouncer
					currentLength={currentLength}
					maxLength={maxLength}
					data-flx="ui.character-counter.character-counter.character-count-announcer"
				/>
			</>
		);
	},
);
