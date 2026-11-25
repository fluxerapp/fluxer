// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/embeds/AltTextBadge.module.css';
import {AltTextTooltip} from '@app/features/ui/alt_text_tooltip/AltTextTooltip';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Trans} from '@lingui/react/macro';
import type {FC} from 'react';

interface AltTextBadgeProps {
	altText?: string | null;
	onPopoutToggle?: (open: boolean) => void;
}

const getAltBadgeText = (altText?: string | null): string | null => {
	if (!altText) return null;
	const trimmed = altText.trim();
	return trimmed.length > 0 ? trimmed : null;
};
export const AltTextBadge: FC<AltTextBadgeProps> = ({altText, onPopoutToggle}) => {
	const badgeText = getAltBadgeText(altText);
	if (!badgeText) return null;
	return (
		<div className={styles.wrapper} data-flx="channel.embeds.alt-text-badge.wrapper">
			<AltTextTooltip
				altText={badgeText}
				onPopoutToggle={onPopoutToggle}
				data-flx="channel.embeds.alt-text-badge.alt-text-tooltip"
			>
				<FocusRing offset={-2} data-flx="channel.embeds.alt-text-badge.focus-ring">
					<button
						type="button"
						className={styles.button}
						aria-label={badgeText}
						data-flx="channel.embeds.alt-text-badge.button"
					>
						<Trans
							context="alt text badge"
							comment="Short badge on an image or video that has alt text. Opens the description on hover. Use the short abbreviation readers in your language know for alt text, at most four letters."
						>
							ALT
						</Trans>
					</button>
				</FocusRing>
			</AltTextTooltip>
		</div>
	);
};
