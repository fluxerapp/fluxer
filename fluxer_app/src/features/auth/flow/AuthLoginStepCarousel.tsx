// SPDX-License-Identifier: AGPL-3.0-or-later

import {DesktopHandoffMfaStep} from '@app/features/auth/flow/DesktopHandoffMfaStep';
import Authentication, {LoginState} from '@app/features/auth/state/Authentication';
import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {observer} from 'mobx-react-lite';
import type {ReactNode} from 'react';

const STEP_ORDER: ReadonlyArray<LoginState> = [LoginState.DEFAULT, LoginState.MFA];

interface AuthLoginStepCarouselProps {
	readonly ariaLabel: string;
	readonly mfaFallbackRedirectPath: string;
	readonly onLoginComplete: () => void;
	readonly carouselDataFlx: string;
	readonly mfaStepDataFlx: string;
	readonly children: ReactNode;
}

export const AuthLoginStepCarousel = observer(function AuthLoginStepCarousel({
	ariaLabel,
	mfaFallbackRedirectPath,
	onLoginComplete,
	carouselDataFlx,
	mfaStepDataFlx,
	children,
}: AuthLoginStepCarouselProps) {
	return (
		<SteppedCarousel
			step={Authentication.loginState}
			steps={STEP_ORDER}
			focusOnStepChange
			ariaLabel={ariaLabel}
			data-flx={carouselDataFlx}
		>
			{Authentication.loginState === LoginState.MFA ? (
				<DesktopHandoffMfaStep
					fallbackRedirectPath={mfaFallbackRedirectPath}
					onLoginComplete={onLoginComplete}
					data-flx={mfaStepDataFlx}
				/>
			) : (
				children
			)}
		</SteppedCarousel>
	);
});
