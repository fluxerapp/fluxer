// SPDX-License-Identifier: AGPL-3.0-or-later

import {AccountProfilePicker} from '@app/features/auth/components/accounts/account_switcher_modal/AccountProfilePicker';
import {
	type AccountSwitcherContentView,
	AccountSwitcherView,
} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherModalTypes';
import styles from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherOverlay.module.css';
import {AccountSwitcherViewTransition} from '@app/features/auth/components/accounts/account_switcher_modal/AccountSwitcherViewTransition';
import type {Account} from '@app/features/platform/state/AuthSession';
import FocusRingScope from '@app/features/ui/focus_ring/FocusRingScope';
import {usePrefersReducedMotion} from '@app/features/ui/hooks/usePrefersReducedMotion';
import {AnimeButton, type AnimeTarget, createAnimeFlxElement} from '@app/features/ui/motion/AnimeElement';
import LayerManager, {LayerType} from '@app/features/ui/state/LayerManager';
import {getZIndexForStack} from '@app/features/ui/state/Modal';
import {ModalStackContext} from '@app/features/ui/utils/ModalStackContext';
import {isDesktop} from '@app/features/ui/utils/NativeUtils';
import {flxElementClassName} from '@app/lib/react';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import type React from 'react';
import {useCallback, useContext, useEffect, useId, useRef} from 'react';

const AccountSwitcherAnimatedRoot = createAnimeFlxElement('flx-auth-account-switcher-overlay');
const AccountSwitcherAnimatedShell = createAnimeFlxElement('flx-auth-account-switcher-overlay-shell');

const CLOSE_ACCOUNT_SWITCHER_DESCRIPTOR = msg({
	message: 'Close account switcher',
	comment: 'Accessible label for closing the account switcher overlay.',
});
const BACK_TO_ACCOUNT_SWITCHER_DESCRIPTOR = msg({
	message: 'Back to account switcher',
	comment: 'Accessible label for returning from the account switcher authentication form to the account list.',
});

function isAccountShortcutKey(key: string): boolean {
	return key.length === 1 && key >= '1' && key <= '9';
}

interface AccountSwitcherStageProps {
	readonly accounts: Array<Account>;
	readonly authContent: React.ReactNode;
	readonly currentAccountKey: string | null;
	readonly onAccountClick: (account: Account) => void;
	readonly onAccountRemove: (account: Account) => void;
	readonly onAddAccount: () => void;
	readonly pickerDisabled: boolean;
	readonly view: AccountSwitcherContentView;
}

function AccountSwitcherStage({
	accounts,
	authContent,
	currentAccountKey,
	onAccountClick,
	onAccountRemove,
	onAddAccount,
	pickerDisabled,
	view,
}: AccountSwitcherStageProps): React.ReactElement {
	if (view === AccountSwitcherView.AUTH) {
		return (
			<flx-auth-account-switcher-overlay-auth-stage
				className={flxElementClassName(styles.authStage)}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-overlay.account-switcher-stage.auth-stage"
			>
				{authContent}
			</flx-auth-account-switcher-overlay-auth-stage>
		);
	}
	return (
		<flx-auth-account-switcher-overlay-accounts-stage
			className={flxElementClassName(styles.accountsStage)}
			data-flx="auth.accounts.account-switcher-modal.account-switcher-overlay.account-switcher-stage.accounts-stage"
		>
			{accounts.length === 0 ? (
				<flx-auth-account-switcher-overlay-empty
					className={flxElementClassName(styles.noAccounts)}
					data-flx="auth.accounts.account-switcher-modal.account-switcher-overlay.account-switcher-stage.no-accounts"
				>
					<Trans>No accounts</Trans>
				</flx-auth-account-switcher-overlay-empty>
			) : null}
			<AccountProfilePicker
				accounts={accounts}
				currentAccountKey={currentAccountKey}
				disabled={pickerDisabled}
				showInstance={isDesktop()}
				primaryActionDisabled={false}
				onPrimaryAction={onAccountClick}
				secondaryActionDisabled={pickerDisabled}
				onSecondaryAction={onAccountRemove}
				onAddAccount={onAddAccount}
				className={styles.accountPicker}
				scrollerClassName={styles.accountScroller}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-overlay.account-switcher-stage.account-picker"
			/>
		</flx-auth-account-switcher-overlay-accounts-stage>
	);
}

interface AccountSwitcherOverlayProps {
	readonly ariaLabel: string;
	readonly view: AccountSwitcherContentView;
	readonly accounts: Array<Account>;
	readonly currentAccountKey: string | null;
	readonly isBusy: boolean;
	readonly authContent: React.ReactNode;
	readonly onAccountClick: (account: Account) => void;
	readonly onAccountRemove: (account: Account) => void;
	readonly onAddAccount: () => void;
	readonly onBackdropClick: () => void;
}

export function AccountSwitcherOverlay({
	ariaLabel,
	view,
	accounts,
	currentAccountKey,
	isBusy,
	authContent,
	onAccountClick,
	onAccountRemove,
	onAddAccount,
	onBackdropClick,
}: AccountSwitcherOverlayProps): React.ReactElement {
	const {i18n} = useLingui();
	const prefersReducedMotion = usePrefersReducedMotion();
	const shellRef = useRef<HTMLElement | null>(null);
	const layerKey = useId();
	const {stackIndex, isVisible, isTopmost} = useContext(ModalStackContext);
	const closeLabel = i18n._(CLOSE_ACCOUNT_SWITCHER_DESCRIPTOR);
	let backdropLabel = closeLabel;
	if (view === AccountSwitcherView.AUTH) {
		backdropLabel = i18n._(BACK_TO_ACCOUNT_SWITCHER_DESCRIPTOR);
	}
	const handleShellKeyDown = useCallback(
		(event: React.KeyboardEvent<HTMLElement>) => {
			if (view !== AccountSwitcherView.ACCOUNTS || isBusy) {
				return;
			}
			if (event.altKey || event.ctrlKey || event.metaKey || event.shiftKey) {
				return;
			}
			if (!isAccountShortcutKey(event.key)) {
				return;
			}
			const account = accounts[Number(event.key) - 1];
			if (account == null) {
				return;
			}
			event.preventDefault();
			event.stopPropagation();
			onAccountClick(account);
		},
		[accounts, isBusy, onAccountClick, view],
	);

	useEffect(() => {
		LayerManager.addLayer(LayerType.MODAL, layerKey, onBackdropClick);
		return () => LayerManager.removeLayer(LayerType.MODAL, layerKey);
	}, [layerKey, onBackdropClick]);

	useEffect(() => {
		if (!isTopmost) {
			return;
		}
		shellRef.current?.focus({preventScroll: true});
	}, [isTopmost, view]);

	let pointerEvents: React.CSSProperties['pointerEvents'] = 'none';
	let visibility: React.CSSProperties['visibility'] = 'hidden';
	let opacity = 0;
	if (isVisible) {
		pointerEvents = 'auto';
		visibility = 'visible';
		opacity = 1;
	}
	let rootTweenDuration = 0;
	let backdropTweenDuration = 0;
	let shellTweenDuration = 0;
	let fadeFrom: AnimeTarget | false = false;
	let fadeLeave: AnimeTarget | false = false;
	let shellFrom: AnimeTarget | false = false;
	let shellLeave: AnimeTarget | false = false;
	if (!prefersReducedMotion) {
		rootTweenDuration = 0.16;
		backdropTweenDuration = 0.18;
		shellTweenDuration = 0.24;
		fadeFrom = {opacity: 0};
		fadeLeave = {opacity: 0};
		shellFrom = {opacity: 0, translateY: 16, scale: 0.985};
		shellLeave = {opacity: 0, translateY: 8, scale: 0.99};
	}

	return (
		<AccountSwitcherAnimatedRoot
			className={styles.root}
			style={{pointerEvents, visibility, zIndex: getZIndexForStack(stackIndex)}}
			from={fadeFrom}
			to={{opacity}}
			leave={fadeLeave}
			tween={{duration: rootTweenDuration, ease: 'out(3)'}}
			data-flx="auth.accounts.account-switcher-modal.account-switcher-overlay.root"
		>
			<AnimeButton
				type="button"
				className={styles.backdrop}
				aria-label={backdropLabel}
				tabIndex={-1}
				onClick={onBackdropClick}
				from={fadeFrom}
				to={{opacity: 1}}
				leave={fadeLeave}
				tween={{duration: backdropTweenDuration, ease: 'out(3)'}}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-overlay.backdrop.button"
			/>
			<AccountSwitcherAnimatedShell
				ref={shellRef}
				className={styles.shell}
				role="dialog"
				aria-modal="true"
				aria-label={ariaLabel}
				aria-busy={isBusy}
				tabIndex={-1}
				onKeyDown={handleShellKeyDown}
				from={shellFrom}
				to={{opacity: 1, translateY: 0, scale: 1}}
				leave={shellLeave}
				tween={{duration: shellTweenDuration, ease: 'out(4)'}}
				data-flx="auth.accounts.account-switcher-modal.account-switcher-overlay.shell"
			>
				<FocusRingScope
					containerRef={shellRef}
					data-flx="auth.accounts.account-switcher-modal.account-switcher-overlay.focus-ring-scope"
				>
					<AccountSwitcherViewTransition
						transitionKey={view}
						className={styles.viewHost}
						data-flx="auth.accounts.account-switcher-modal.account-switcher-overlay.view-host"
					>
						<AccountSwitcherStage
							accounts={accounts}
							authContent={authContent}
							currentAccountKey={currentAccountKey}
							onAccountClick={onAccountClick}
							onAccountRemove={onAccountRemove}
							onAddAccount={onAddAccount}
							pickerDisabled={isBusy}
							view={view}
							data-flx="auth.accounts.account-switcher-modal.account-switcher-overlay.account-switcher-stage"
						/>
					</AccountSwitcherViewTransition>
				</FocusRingScope>
			</AccountSwitcherAnimatedShell>
		</AccountSwitcherAnimatedRoot>
	);
}
