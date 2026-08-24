// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import styles from '@app/features/ui/components/MentionBadge.module.css';
import {
	AnimePresence,
	type AnimeTarget,
	type AnimeTween,
	AnimeTweenType,
	createAnimeFlxElement,
} from '@app/features/ui/motion/AnimeElement';
import {getCurrentLocale} from '@app/features/user/utils/LocaleUtils';
import {formatCompactNumber, formatNumber} from '@pkgs/number_utils/src/NumberFormatting';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';

const MentionBadgeSurface = createAnimeFlxElement('flx-ui-mention-badge-surface');

const HIDDEN_MENTION_BADGE: AnimeTarget = Object.freeze({opacity: 0, scale: 0.85});
const VISIBLE_MENTION_BADGE: AnimeTarget = Object.freeze({opacity: 1, scale: 1});

interface MentionBadgeMotion {
	readonly from: AnimeTarget;
	readonly to: AnimeTarget;
	readonly tween: AnimeTween;
}

const MENTION_BADGE_LEAVE = false as const;

const STANDARD_MENTION_BADGE_MOTION: MentionBadgeMotion = Object.freeze({
	from: HIDDEN_MENTION_BADGE,
	to: VISIBLE_MENTION_BADGE,
	tween: Object.freeze({type: AnimeTweenType.SPRING, stiffness: 500, damping: 22}),
});
const REDUCED_MENTION_BADGE_MOTION: MentionBadgeMotion = Object.freeze({
	from: VISIBLE_MENTION_BADGE,
	to: VISIBLE_MENTION_BADGE,
	tween: Object.freeze({duration: 0}),
});

function resolveMentionBadgeMotion(useReducedMotion: boolean): MentionBadgeMotion {
	if (useReducedMotion) {
		return REDUCED_MENTION_BADGE_MOTION;
	}
	return STANDARD_MENTION_BADGE_MOTION;
}

const formatMentionCount = (mentionCount: number) => {
	const locale = getCurrentLocale();
	if (mentionCount > 99 && mentionCount < 1000) {
		return '99+';
	}
	if (mentionCount >= 1000) {
		return formatCompactNumber(mentionCount, locale, 0).replace(/\s/g, '');
	}
	return formatNumber(mentionCount, locale);
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
		<div
			className={clsx(styles.badge, size === 'small' ? styles.badgeSmall : styles.badgeMedium)}
			data-flx="ui.mention-badge.badge"
		>
			{formatMentionCount(mentionCount)}
		</div>
	);
});
export const MentionBadgeAnimated = observer(({mentionCount, size = 'medium'}: MentionBadgeProps) => {
	const motion = resolveMentionBadgeMotion(Accessibility.useReducedMotion);
	return (
		<AnimePresence enterOnMount={false} data-flx="ui.mention-badge.mention-badge-animated.anime-presence">
			{mentionCount > 0 && (
				<MentionBadgeSurface
					className={styles.animatedWrapper}
					from={motion.from}
					to={motion.to}
					leave={MENTION_BADGE_LEAVE}
					tween={motion.tween}
					data-flx="ui.mention-badge.mention-badge-animated.animated-wrapper"
				>
					<MentionBadge
						mentionCount={mentionCount}
						size={size}
						data-flx="ui.mention-badge.mention-badge-animated.mention-badge"
					/>
				</MentionBadgeSurface>
			)}
		</AnimePresence>
	);
});
