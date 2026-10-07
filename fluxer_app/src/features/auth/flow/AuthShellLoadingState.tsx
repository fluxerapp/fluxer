// SPDX-License-Identifier: AGPL-3.0-or-later

import {welcomeTextForLocale} from '@app/features/app/components/setup/SetupWizardWelcomeRotation';
import {
	AuthShellHintKind,
	type AuthShellPlaceholder,
	readAuthShellPlaceholderEnvironment,
	resolveAuthShellPlaceholder,
	resolveAuthShellRowCount,
} from '@app/features/app/components/skeleton/AuthShellHint';
import {AuthLoadingState} from '@app/features/auth/flow/AuthLoadingState';
import {AuthLayoutContentMode, useAuthLayoutContext} from '@app/features/auth/state/AuthLayoutContext';
import {useLocation} from '@app/features/platform/components/router/RouterReact';
import {getRemScaleForDocument} from '@app/features/theme/layout/RemFromPx';
import {isMobileExperienceEnabled} from '@app/features/ui/utils/MobileExperience';
import * as LocaleUtils from '@app/features/user/utils/LocaleUtils';
import type React from 'react';
import {type JSX, useLayoutEffect, useState} from 'react';

const SHIMMER_DURATION_MS = 3200;
const CARD_CHROME = '2 * clamp(2rem, 4vw, 2.75rem) - 0.125rem';
const BLOCK = 'fluxer-boot-block';

interface AuthShellLoadingModel {
	readonly mobile: boolean;
	readonly placeholder: AuthShellPlaceholder | null;
	readonly rows: number;
	readonly shimmerDelayMs: number;
}

function remFromPx(px: number): string {
	return `${Math.round((px / 16) * 1e5) / 1e5}rem`;
}

function resolveModel(pathname: string): AuthShellLoadingModel {
	const mobile = isMobileExperienceEnabled();
	const placeholder = resolveAuthShellPlaceholder(pathname, readAuthShellPlaceholderEnvironment(mobile));
	return {
		mobile,
		placeholder,
		rows:
			placeholder == null
				? 0
				: resolveAuthShellRowCount(placeholder, mobile, window.innerWidth, getRemScaleForDocument(document)),
		shimmerDelayMs: -(performance.now() % SHIMMER_DURATION_MS),
	};
}

export function AuthShellLoadingState(): JSX.Element {
	const location = useLocation();
	const {setContentMode} = useAuthLayoutContext();
	const [model] = useState(() => resolveModel(location.pathname));
	const full = model.placeholder?.kind === AuthShellHintKind.FULL;
	useLayoutEffect(() => {
		if (!full) {
			return;
		}
		setContentMode(AuthLayoutContentMode.FULL);
		return () => setContentMode(AuthLayoutContentMode.CARD);
	}, [full, setContentMode]);
	const placeholder = model.placeholder;
	if (placeholder == null) {
		return <AuthLoadingState data-flx="auth.flow.auth-shell-loading-state.auth-loading-state" />;
	}
	const shimmerStyle = {'--skeleton-shimmer-delay': `${model.shimmerDelayMs}ms`} as React.CSSProperties;
	if (full) {
		return (
			<div
				className="fluxer-boot-auth-full"
				style={shimmerStyle}
				aria-busy="true"
				data-flx="auth.flow.auth-shell-loading-state.full"
			>
				<div className="fluxer-boot-auth-word" data-flx="auth.flow.auth-shell-loading-state.word">
					<div className="fluxer-boot-auth-wtext" data-flx="auth.flow.auth-shell-loading-state.word-text">
						{welcomeTextForLocale(LocaleUtils.getCurrentOrDetectedLocale())}
					</div>
				</div>
				<div className="fluxer-boot-auth-foot" data-flx="auth.flow.auth-shell-loading-state.foot">
					<div
						className={`${BLOCK} fluxer-boot-rm`}
						style={{width: '10rem', height: '2.25rem'}}
						data-flx="auth.flow.auth-shell-loading-state.foot-block"
					/>
				</div>
			</div>
		);
	}
	const height = model.mobile
		? remFromPx(placeholder.heightPx)
		: `calc(${remFromPx(placeholder.heightPx)} - ${CARD_CHROME})`;
	return (
		<div
			className="fluxer-boot-auth-load"
			style={{...shimmerStyle, height}}
			aria-busy="true"
			data-mobile={model.mobile ? '1' : '0'}
			data-flx="auth.flow.auth-shell-loading-state.card"
		>
			<div className="fluxer-boot-auth-title" data-flx="auth.flow.auth-shell-loading-state.title">
				<div
					className={`${BLOCK} fluxer-boot-rp fluxer-boot-s`}
					style={{width: placeholder.category === 'register' ? '11rem' : '8.5rem', height: '1.125rem'}}
					data-flx="auth.flow.auth-shell-loading-state.title-block"
				/>
			</div>
			<div className="fluxer-boot-auth-list" data-flx="auth.flow.auth-shell-loading-state.list">
				{Array.from({length: model.rows}, (_unused, index) => (
					<div
						key={index}
						className={`${BLOCK} fluxer-boot-rm ${index === 0 ? 'fluxer-boot-auth-primary' : 'fluxer-boot-m'}`}
						style={{width: '100%', height: '2.25rem'}}
						data-flx="auth.flow.auth-shell-loading-state.row"
					/>
				))}
			</div>
		</div>
	);
}
