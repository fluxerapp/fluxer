// SPDX-License-Identifier: AGPL-3.0-or-later

const PAGE_TITLE_ID = '_top';

class FluxerStarlightTOC extends HTMLElement {
	private _current = this.querySelector<HTMLAnchorElement>('a[aria-current="true"]');
	private minH = Number.parseInt(this.dataset.minH || '2', 10);
	private maxH = Number.parseInt(this.dataset.maxH || '3', 10);

	private tocHeadingSelector =
		`h1#${PAGE_TITLE_ID},` +
		`:where(${[...Array.from({length: 1 + this.maxH - this.minH}).map((_, index) => `h${this.minH + index}`)].join()})[id]`;

	protected set current(link: HTMLAnchorElement) {
		if (link === this._current) {
			return;
		}
		if (this._current) {
			this._current.removeAttribute('aria-current');
		}
		link.setAttribute('aria-current', 'true');
		this._current = link;
	}

	private onIdle = (cb: IdleRequestCallback) => (window.requestIdleCallback || ((cb) => setTimeout(cb, 1)))(cb);

	constructor() {
		super();
		this.markInitialEntry();
		this.onIdle(() => this.init());
	}

	private markInitialEntry(): void {
		const links = [...this.querySelectorAll('a')];
		if (links.length === 0) {
			return;
		}
		const navBarHeight = document.querySelector('header')?.getBoundingClientRect().height || 0;
		const mobileTocHeight = this.querySelector('summary')?.getBoundingClientRect().height || 0;
		const readingLine = navBarHeight + mobileTocHeight + 32;
		let match: HTMLAnchorElement | undefined;
		for (const heading of document.querySelectorAll<HTMLElement>(this.tocHeadingSelector)) {
			if (heading.getBoundingClientRect().top > readingLine) {
				break;
			}
			const link = links.find((candidate) => candidate.hash === `#${encodeURIComponent(heading.id)}`);
			if (link) {
				match = link;
			}
		}
		this.current = match ?? links[0];
	}

	private init = (): void => {
		const links = [...this.querySelectorAll('a')];

		const isHeading = (el: Element): el is HTMLHeadingElement => el.matches(this.tocHeadingSelector);

		const getElementHeading = (el: Element | null): HTMLHeadingElement | null => {
			if (!el) {
				return null;
			}
			const origin = el;
			while (el) {
				if (el.matches('.sl-markdown-content, main > *')) {
					return document.getElementById(PAGE_TITLE_ID) as HTMLHeadingElement;
				}
				if (isHeading(el)) {
					return el;
				}
				const childHeading = el.querySelector<HTMLHeadingElement>(this.tocHeadingSelector);
				if (childHeading) {
					return childHeading;
				}
				el = el.previousElementSibling;
				while (el?.lastElementChild) {
					el = el.lastElementChild;
				}
				const h = getElementHeading(el);
				if (h) {
					return h;
				}
			}
			return getElementHeading(origin.parentElement);
		};

		const setCurrent: IntersectionObserverCallback = (entries) => {
			for (const {isIntersecting, target} of entries) {
				if (!isIntersecting) {
					continue;
				}
				const heading = getElementHeading(target);
				if (!heading) {
					continue;
				}
				const link = links.find((candidate) => candidate.hash === `#${encodeURIComponent(heading.id)}`);
				if (link) {
					this.current = link;
					break;
				}
			}
		};

		const toObserve = document.querySelectorAll(
			[
				`main :where(${this.tocHeadingSelector})`,
				`main :where(${this.tocHeadingSelector}, .sl-heading-wrapper) ~ *:not(:has(${this.tocHeadingSelector}))`,
				`main .sl-markdown-content > *:not(:has(${this.tocHeadingSelector}))`,
				`main > *:not(:has(${this.tocHeadingSelector}))`,
			].join(),
		);

		let observer: IntersectionObserver | undefined;
		const observe = () => {
			if (observer) {
				return;
			}
			observer = new IntersectionObserver(setCurrent, {rootMargin: this.getRootMargin()});
			for (const h of toObserve) {
				observer.observe(h);
			}
		};
		observe();

		let timeout: ReturnType<typeof setTimeout>;
		window.addEventListener('resize', () => {
			if (observer) {
				observer.disconnect();
				observer = undefined;
			}
			clearTimeout(timeout);
			timeout = setTimeout(() => this.onIdle(observe), 200);
		});
	};

	private getRootMargin(): `-${number}px 0% ${number}px` {
		const navBarHeight = document.querySelector('header')?.getBoundingClientRect().height || 0;
		const mobileTocHeight = this.querySelector('summary')?.getBoundingClientRect().height || 0;
		const top = navBarHeight + mobileTocHeight + 32;
		const bottom = top + 53;
		const height = document.documentElement.clientHeight;
		return `-${top}px 0% ${bottom - height}px`;
	}
}

if (!customElements.get('starlight-toc')) {
	customElements.define('starlight-toc', FluxerStarlightTOC);
}
