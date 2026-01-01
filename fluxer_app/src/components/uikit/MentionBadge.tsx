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

import {clsx} from 'clsx';
import {AnimatePresence, motion} from 'framer-motion';
import {observer} from 'mobx-react-lite';
import AccessibilityStore from '~/stores/AccessibilityStore';
import {getCurrentLocale} from '~/utils/LocaleUtils';
import styles from './MentionBadge.module.css';

const formatMentionCount = (mentionCount: number) => {
	const locale = getCurrentLocale();

	if (mentionCount > 99 && mentionCount < 1000) {
		return '99+';
	}

	if (mentionCount >= 1000) {
		const formatter = new Intl.NumberFormat(locale, {
			notation: 'compact',
			maximumFractionDigits: 0,
		});
		return formatter.format(mentionCount).replace(/\s/g, '');
	}

	return new Intl.NumberFormat(locale).format(mentionCount);
};

interface MentionBadgeProps {
	mentionCount: number;
	size?: 'small' | 'medium';
}

export const MentionBadge = observer(({mentionCount, size = 'medium'}: MentionBadgeProps) => {
	if (mentionCount === 0) {
		return null;
	}

	return (
		<div className={clsx(styles.badge, size === 'small' ? styles.badgeSmall : styles.badgeMedium)}>
			{formatMentionCount(mentionCount)}
		</div>
	);
});

export const MentionBadgeAnimated = observer(({mentionCount, size = 'medium'}: MentionBadgeProps) => {
	const shouldAnimate = !AccessibilityStore.useReducedMotion;

	if (!shouldAnimate) {
		return mentionCount > 0 ? <MentionBadge mentionCount={mentionCount} size={size} /> : null;
	}

	return (
		<AnimatePresence initial={false} mode="wait">
			{mentionCount > 0 && (
				<motion.div
					initial={{opacity: 0, scale: 0.85}}
					animate={{opacity: 1, scale: 1}}
					exit={{opacity: 0, scale: 0.85}}
					transition={{type: 'spring', stiffness: 500, damping: 22}}
				>
					<MentionBadge mentionCount={mentionCount} size={size} />
				</motion.div>
			)}
		</AnimatePresence>
	);
});
