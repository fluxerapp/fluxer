// SPDX-License-Identifier: AGPL-3.0-or-later

import {usePrefersReducedMotion} from '@app/features/ui/hooks/usePrefersReducedMotion';
import {AnimePresence, createAnimeFlxElement} from '@app/features/ui/motion/AnimeElement';
import {flxElementClassName} from '@app/lib/react';
import type React from 'react';
import {useEffect, useRef, useState} from 'react';

const AccountSwitcherAnimatedView = createAnimeFlxElement('flx-auth-account-switcher-overlay-view');

const VIEW_TRANSITION_DURATION_SECONDS = 0.18;
const VIEW_TRANSITION_OFFSET_PX = 12;
const VIEW_TRANSITION_DURATION_MS = VIEW_TRANSITION_DURATION_SECONDS * 1000;

interface AccountSwitcherViewTransitionProps {
	readonly transitionKey: string;
	readonly className: string;
	readonly children: React.ReactNode;
}

interface DisplayedView {
	readonly key: string;
	readonly children: React.ReactNode;
}

function useSequencedView(transitionKey: string, children: React.ReactNode): DisplayedView | null {
	const [displayed, setDisplayed] = useState<DisplayedView>({key: transitionKey, children});
	const [isExiting, setIsExiting] = useState(false);
	const pendingRef = useRef<DisplayedView>({key: transitionKey, children});
	pendingRef.current = {key: transitionKey, children};

	useEffect(() => {
		if (transitionKey === displayed.key) {
			setDisplayed({key: transitionKey, children});
			setIsExiting(false);
			return;
		}
		setIsExiting(true);
		const timer = setTimeout(() => {
			setDisplayed(pendingRef.current);
			setIsExiting(false);
		}, VIEW_TRANSITION_DURATION_MS);
		return () => clearTimeout(timer);
	}, [transitionKey, children, displayed.key]);

	return isExiting ? null : displayed;
}

function renderAnimatedView(displayed: DisplayedView | null): React.ReactElement {
	return (
		<AnimePresence
			enterOnMount={false}
			data-flx="auth.accounts.account-switcher-modal.account-switcher-view-transition.anime-presence"
		>
			{displayed == null ? null : (
				<AccountSwitcherAnimatedView
					key={displayed.key}
					from={{opacity: 0, translateY: VIEW_TRANSITION_OFFSET_PX}}
					to={{opacity: 1, translateY: 0}}
					leave={{opacity: 0, translateY: -VIEW_TRANSITION_OFFSET_PX}}
					tween={{duration: VIEW_TRANSITION_DURATION_SECONDS, ease: 'out(3)'}}
					data-flx="auth.accounts.account-switcher-modal.account-switcher-view-transition.animated-view"
				>
					{displayed.children}
				</AccountSwitcherAnimatedView>
			)}
		</AnimePresence>
	);
}

function renderStaticView(children: React.ReactNode): React.ReactElement {
	return (
		<flx-auth-account-switcher-overlay-view
			className={flxElementClassName()}
			data-flx="auth.accounts.account-switcher-modal.account-switcher-view-transition.static-view"
		>
			{children}
		</flx-auth-account-switcher-overlay-view>
	);
}

export function AccountSwitcherViewTransition({
	transitionKey,
	className,
	children,
}: AccountSwitcherViewTransitionProps): React.ReactElement {
	const prefersReducedMotion = usePrefersReducedMotion();
	const displayed = useSequencedView(transitionKey, children);
	return (
		<flx-auth-account-switcher-overlay-view-host
			className={flxElementClassName(className)}
			data-flx="auth.accounts.account-switcher-modal.account-switcher-view-transition.view-host"
		>
			{prefersReducedMotion ? renderStaticView(children) : renderAnimatedView(displayed)}
		</flx-auth-account-switcher-overlay-view-host>
	);
}
