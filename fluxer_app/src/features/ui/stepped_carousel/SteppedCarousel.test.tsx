// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {SteppedCarousel} from '@app/features/ui/stepped_carousel/SteppedCarousel';
import {act} from 'react';
import {createRoot} from 'react-dom/client';
import {expect, test, vi} from 'vitest';

(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;

test('reports the step whose pane was just shown', () => {
	const container = document.createElement('div');
	document.body.append(container);
	const root = createRoot(container);
	const onStepShown = vi.fn();
	const render = (step: 'loading' | 'ready') =>
		act(() => {
			root.render(
				<SteppedCarousel
					step={step}
					steps={[step]}
					onStepShown={onStepShown}
					data-flx="ui.stepped-carousel.test.carousel"
				>
					<span data-flx="ui.stepped-carousel.test.step">{step}</span>
				</SteppedCarousel>,
			);
		});
	render('loading');
	expect(onStepShown).toHaveBeenLastCalledWith('loading');
	render('ready');
	expect(onStepShown).toHaveBeenLastCalledWith('ready');
	act(() => root.unmount());
	container.remove();
});
