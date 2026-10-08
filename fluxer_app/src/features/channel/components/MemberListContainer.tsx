// SPDX-License-Identifier: AGPL-3.0-or-later

import {useRovingFocusList} from '@app/features/app/hooks/useRovingFocusList';
import styles from '@app/features/channel/components/MemberListContainer.module.css';
import {MEMBERS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import KeyboardMode from '@app/features/ui/state/KeyboardMode';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import type {UIEvent} from 'react';

type ScrollerResizeType = 'container' | 'content';

interface MemberListContainerProps {
	channelId: string;
	identityKey?: string;
	children: React.ReactNode;
	className?: string;
	scrollerRef?: React.RefObject<ScrollerHandle | null>;
	estimatedContentSize?: number | null;
	onScroll?: (event: UIEvent<HTMLDivElement>) => void;
	onResize?: (entry: ResizeObserverEntry, type: ScrollerResizeType) => void;
}

export const MemberListContainer: React.FC<MemberListContainerProps> = observer(function MemberListContainer({
	channelId,
	identityKey,
	children,
	className,
	scrollerRef,
	estimatedContentSize,
	onScroll,
	onResize,
}) {
	const {i18n} = useLingui();
	const navigationRef = useRovingFocusList<HTMLElement>({
		focusableSelector: '[data-member-list-focus-item="true"]',
		orientation: 'vertical',
		loop: true,
		enabled: KeyboardMode.keyboardModeEnabled,
		restoreFocusOnWindowFocus: false,
		manageTabIndex: true,
	});
	return (
		<aside
			ref={navigationRef}
			className={clsx(styles.memberListContainer, className)}
			aria-label={i18n._(MEMBERS_DESCRIPTOR)}
			data-flx="channel.member-list-container.member-list-container"
		>
			<Scroller
				ref={scrollerRef}
				className={styles.memberListScroller}
				key={`member-list-scroller-${identityKey ?? channelId}`}
				onScroll={onScroll}
				onResize={onResize}
				estimatedContentSize={estimatedContentSize}
				data-flx="channel.member-list-container.member-list-scroller"
			>
				{children}
			</Scroller>
		</aside>
	);
});
