// SPDX-License-Identifier: AGPL-3.0-or-later

import authLayoutStyles from '@app/features/app/components/layout/AuthLayout.module.css';
import styles from '@app/features/auth/flow/AuthCardContainer.module.css';
import {type AuthCardVariant, AuthLayoutContentMode} from '@app/features/auth/state/AuthLayoutContext';
import {flxElementClassName} from '@app/lib/react';
import clsx from 'clsx';
import type {ReactNode} from 'react';

interface AuthCardContainerProps {
	variant?: AuthCardVariant;
	presentation?: AuthLayoutContentMode;
	children: ReactNode;
	isInert?: boolean;
	className?: string;
	cardClassName?: string;
	contentClassName?: string;
}

const cardVariantClassNames: Record<AuthCardVariant, string | undefined> = {
	default: undefined,
	standard: authLayoutStyles.cardStandard,
	compact: authLayoutStyles.cardCompact,
	wide: authLayoutStyles.cardWide,
};
const formSideVariantClassNames: Record<AuthCardVariant, string | undefined> = {
	default: undefined,
	standard: authLayoutStyles.formSideStandard,
	compact: authLayoutStyles.formSideCompact,
	wide: authLayoutStyles.formSideWide,
};

function resolveContainerClassName(isFullscreen: boolean, className: string | undefined): string {
	if (isFullscreen) {
		return authLayoutStyles.fullscreenPassthrough;
	}
	return clsx(authLayoutStyles.cardContainer, className);
}

function resolveSurfaceClassName(
	isFullscreen: boolean,
	variant: AuthCardVariant,
	cardClassName: string | undefined,
): string {
	if (isFullscreen) {
		return authLayoutStyles.fullscreenPassthrough;
	}
	return clsx(authLayoutStyles.card, cardVariantClassNames[variant], cardClassName);
}

function resolveFormSideClassName(
	isFullscreen: boolean,
	variant: AuthCardVariant,
	contentClassName: string | undefined,
): string {
	if (isFullscreen) {
		return authLayoutStyles.fullscreenPassthrough;
	}
	return clsx(authLayoutStyles.formSide, formSideVariantClassNames[variant], contentClassName);
}

function renderCardContents(children: ReactNode, isInert: boolean): ReactNode {
	if (!isInert) {
		return children;
	}
	return (
		<flx-auth-card-inert-overlay
			className={flxElementClassName(styles.inertOverlay)}
			data-flx="auth.flow.auth-card-container.inert-overlay"
		>
			{children}
		</flx-auth-card-inert-overlay>
	);
}

export function AuthCardContainer({
	variant = 'default',
	presentation = 'card',
	children,
	isInert = false,
	className,
	cardClassName,
	contentClassName,
}: AuthCardContainerProps) {
	const isFullscreen = presentation === AuthLayoutContentMode.FULL;
	return (
		<flx-auth-card
			className={flxElementClassName(resolveContainerClassName(isFullscreen, className))}
			data-flx="auth.flow.auth-card-container.div"
		>
			<flx-auth-card-surface
				className={flxElementClassName(resolveSurfaceClassName(isFullscreen, variant, cardClassName))}
				data-flx="auth.flow.auth-card-container.div--2"
			>
				<flx-auth-card-form-side
					className={flxElementClassName(resolveFormSideClassName(isFullscreen, variant, contentClassName))}
					data-flx="auth.flow.auth-card-container.div--4"
				>
					{renderCardContents(children, isInert)}
				</flx-auth-card-form-side>
			</flx-auth-card-surface>
		</flx-auth-card>
	);
}
