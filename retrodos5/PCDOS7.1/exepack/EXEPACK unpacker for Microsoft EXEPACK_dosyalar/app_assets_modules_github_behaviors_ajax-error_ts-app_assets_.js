"use strict";(globalThis.webpackChunk=globalThis.webpackChunk||[]).push([["app_assets_modules_github_behaviors_ajax-error_ts-app_assets_modules_github_behaviors_include-f12a82","ui_packages_soft-navigate_soft-navigate_ts"],{54679:(e,t,r)=>{r.d(t,{H:()=>o,v:()=>a});var n=r(59753);function a(){let e=document.getElementById("ajax-error-message");e&&(e.hidden=!1)}function o(){let e=document.getElementById("ajax-error-message");e&&(e.hidden=!0)}(0,n.on)("deprecatedAjaxError","[data-remote]",function(e){let t=e.detail,{error:r,text:n}=t;e.currentTarget===e.target&&"abort"!==r&&"canceled"!==r&&(/<html/.test(n)?(a(),e.stopImmediatePropagation()):setTimeout(function(){e.defaultPrevented||a()},0))}),(0,n.on)("deprecatedAjaxSend","[data-remote]",function(){o()}),(0,n.on)("click",".js-ajax-error-dismiss",function(){o()})},93491:(e,t,r)=>{r.d(t,{I:()=>d,x:()=>s});var n=r(36162),a=r(36071),o=r(59753),i=r(76237);let l=new WeakMap;function d(e){let t=e.closest(".js-render-needs-enrichment");t&&(t.classList.remove("render-error"),l.get(t)?.setLoading(!1))}function s(e,t){let r=e.closest(".js-render-needs-enrichment");return!!r&&(r.classList.add("render-error"),l.get(r)?.setError(!0,t))}function c(e,t,r){let a=r.identifier??"",o=new URL(e,window.location.origin);for(let[e,r]of Object.entries(t))o.searchParams.append(e,`${r}`);return o.hash=a,n.dy`
    <div
      class="render-container color-bg-transparent js-render-target p-0"
      data-identity="${a}"
      data-host="${o.origin}"
      data-type="${r.type}"
    >
      <iframe
        role="presentation"
        class="render-viewer"
        src="${String(o)}"
        name="${a}"
        data-content="${r.contentJson}"
        sandbox="allow-scripts allow-same-origin allow-top-navigation"
      >
      </iframe>
    </div>
  `}function u(e,t,r){let a=n.dy`<clipboard-copy
    aria-label="Copy ${r.type} code"
    .value=${e}
    class="btn my-2 js-clipboard-copy p-0 d-inline-flex tooltipped-no-delay"
    role="button"
    data-copy-feedback="Copied!"
    data-tooltip-direction="s"
  >
    <svg
      aria-hidden="true"
      height="16"
      viewBox="0 0 16 16"
      version="1.1"
      width="16"
      class="octicon octicon-copy js-clipboard-copy-icon m-2"
    >
      <path
        fill-rule="evenodd"
        d="M0 6.75C0 5.784.784 5 1.75 5h1.5a.75.75 0 010 1.5h-1.5a.25.25 0 00-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 00.25-.25v-1.5a.75.75 0 011.5 0v1.5A1.75 1.75 0 019.25 16h-7.5A1.75 1.75 0 010 14.25v-7.5z"
      ></path>
      <path
        fill-rule="evenodd"
        d="M5 1.75C5 .784 5.784 0 6.75 0h7.5C15.216 0 16 .784 16 1.75v7.5A1.75 1.75 0 0114.25 11h-7.5A1.75 1.75 0 015 9.25v-7.5zm1.75-.25a.25.25 0 00-.25.25v7.5c0 .138.112.25.25.25h7.5a.25.25 0 00.25-.25v-7.5a.25.25 0 00-.25-.25h-7.5z"
      ></path>
    </svg>
    <svg
      aria-hidden="true"
      height="16"
      viewBox="0 0 16 16"
      version="1.1"
      width="16"
      class="octicon octicon-check js-clipboard-check-icon color-fg-success d-none m-2"
    >
      <path
        fill-rule="evenodd"
        d="M13.78 4.22a.75.75 0 010 1.06l-7.25 7.25a.75.75 0 01-1.06 0L2.22 9.28a.75.75 0 011.06-1.06L6 10.94l6.72-6.72a.75.75 0 011.06 0z"
      ></path>
    </svg>
  </clipboard-copy>`,o=n.dy`
    <details class="details-reset details-overlay details-overlay-dark" style="display: contents">
      <summary
        role="button"
        aria-label="Open dialog"
        class="btn my-2 mr-2 p-0 d-inline-flex"
        aria-haspopup="dialog"
        @click="${t}"
      >
        <svg width="16" height="16" viewBox="0 0 16 16" fill="currentColor" class="octicon m-2">
          <path
            fill-rule="evenodd"
            d="M3.72 3.72a.75.75 0 011.06 1.06L2.56 7h10.88l-2.22-2.22a.75.75 0 011.06-1.06l3.5 3.5a.75.75 0 010 1.06l-3.5 3.5a.75.75 0 11-1.06-1.06l2.22-2.22H2.56l2.22 2.22a.75.75 0 11-1.06 1.06l-3.5-3.5a.75.75 0 010-1.06l3.5-3.5z"
          ></path>
        </svg>
      </summary>
      <details-dialog
        class="Box Box--overlay render-full-screen d-flex flex-column anim-fade-in fast"
        aria-label="${r.type} rendered container"
      >
        <div>
          <button
            aria-label="Close dialog"
            data-close-dialog=""
            type="button"
            data-view-component="true"
            class="Link--muted btn-link position-absolute render-full-screen-close"
          >
            <svg
              width="24"
              height="24"
              viewBox="0 0 24 24"
              fill="currentColor"
              style="display:inline-block;vertical-align:text-bottom"
              class="octicon octicon-x"
            >
              <path
                fill-rule="evenodd"
                d="M5.72 5.72a.75.75 0 011.06 0L12 10.94l5.22-5.22a.75.75 0 111.06 1.06L13.06 12l5.22 5.22a.75.75 0 11-1.06 1.06L12 13.06l-5.22 5.22a.75.75 0 01-1.06-1.06L10.94 12 5.72 6.78a.75.75 0 010-1.06z"
              ></path>
            </svg>
          </button>
          <div class="Box-body border-0" role="presentation"></div>
        </div>
      </details-dialog>
    </details>
  `;return n.dy`<div class="position-absolute top-0 pr-2 width-full d-flex flex-justify-end flex-items-center">
    ${o}${a}
  </div>`}function f(e){let t={color_mode:(0,i.Fg)()},r=e.getAttribute("data-type"),a=e.getAttribute("data-src"),o=e.getAttribute("data-identity"),d=e.getElementsByClassName("js-render-enrichment-target")[0],s=e.getElementsByClassName("js-render-enrichment-loader")[0],f=document.createElement("div");f.classList.add("js-render-enrichment-fallback"),e.appendChild(f);let h=d.firstElementChild;f.appendChild(h);let m={setLoading(e){s.hidden=!e},setError:(e,t)=>(m.setLoading(!1),!1!==e&&(h.classList.toggle("render-plaintext-hidden",!e),!!t&&((0,n.sY)([t,h],f),!0)))};l.set(e,m);let p=d.getAttribute("data-plain"),g=d.getAttribute("data-json");if(null==g||null==p)throw m.setError(!0,n.dy`<p class="flash flash-error">Unable to render rich display</p>`),Error(`Expected to see input data for type: ${r}`);let y=c(a,t,{type:r,identifier:o,contentJson:g}),b=c(a,t,{type:r,identifier:`${o}-fullscreen`,contentJson:g}),v=u(p,()=>{(0,n.sY)(b,d.getElementsByClassName("Box-body")[0])},{type:r});(0,n.sY)([v,y],d)}(0,a.N7)(".js-render-needs-enrichment",{constructor:HTMLElement,initialize:f}),(0,o.on)("preview:toggle:off",".js-previewable-comment-form",function(e){let t=e.currentTarget,r=t.querySelector(".js-render-needs-enrichment"),n=r?.querySelector(".js-render-enrichment-target");n&&(n.textContent="")}),(0,o.on)("preview:rendered",".js-previewable-comment-form",function(e){let t=e.currentTarget,r=t.querySelector(".js-render-needs-enrichment");r&&l.get(r)?.setLoading(!1)})},52191:(e,t,r)=>{r.d(t,{$:()=>c,G:()=>s});var n=r(56959),a=r(36071),o=r(59753);function i(e,t){let r=e.currentTarget;if(r instanceof Element){for(let e of r.querySelectorAll("[data-show-on-error]"))e instanceof HTMLElement&&(e.hidden=!t);for(let e of r.querySelectorAll("[data-hide-on-error]"))e instanceof HTMLElement&&(e.hidden=t)}}function l(e){i(e,!1)}function d(e){i(e,!0)}function s({currentTarget:e}){e instanceof Element&&c(e)}function c(e){let t=e.closest("details");t&&u(t)}function u(e){let t=e.getAttribute("data-deferred-details-content-url");if(t){e.removeAttribute("data-deferred-details-content-url");let r=e.querySelector("include-fragment, poll-include-fragment");r&&(r.src=t)}}(0,a.N7)("include-fragment, poll-include-fragment",{subscribe:e=>(0,n.qC)((0,n.RB)(e,"error",d),(0,n.RB)(e,"loadstart",l))}),(0,o.on)("click","include-fragment button[data-retry-button]",({currentTarget:e})=>{let t=e.closest("include-fragment");t.refetch()})},75297:(e,t,r)=>{r.d(t,{fK:()=>L,j:()=>w,ur:()=>l});var n=r(36162),a=r(93491),o=r(36071),i=r(97629);function l(e){return!!e.querySelector('.js-render-target[data-type="ipynb"]')}let d=["is-render-pending","is-render-ready","is-render-loading","is-render-loaded"],s=["is-render-ready","is-render-loading","is-render-loaded","is-render-failed","is-render-failed-fatally"],c=new WeakMap;function u(e){let t=c.get(e);null!=t&&(t.load=t.hello=null,t.helloTimer&&(clearTimeout(t.helloTimer),t.helloTimer=null),t.loadTimer&&(clearTimeout(t.loadTimer),t.loadTimer=null))}function f(e,t=""){e.classList.remove(...d),e.classList.add("is-render-failed");let r=m(t);!1===(0,a.x)(e,r)&&h(e,r),u(e)}function h(e,t){let r=e.querySelector(".render-viewer-error");r&&(r.remove(),e.classList.remove("render-container"),(0,n.sY)(t,e))}function m(e){let t=n.dy`<p>Unable to render rich display</p>`;if(""!==e){let r=e.split("\n");t=n.dy`<p><b>Unable to render rich display</b></p>
      <p>${r.map(e=>n.dy`${e}<br />`)}</p>`}return n.dy`<div class="flash flash-error">${t}</div>`}function p(e,t=!1){!(!(0,i.Z)(e)||e.classList.contains("is-render-ready")||e.classList.contains("is-render-failed")||e.classList.contains("is-render-failed-fatally"))&&(!t||c.get(e)?.hello)&&f(e)}function g(e,t){return!!e&&!!e.postMessage&&(e.postMessage(JSON.stringify(t),"*"),!0)}function y(e,t){let r=e.querySelector(`.js-render-target[data-identity="${t}"]`);return r&&r instanceof HTMLElement?r:null}function b(e){return t=>{let r=t.querySelector(".js-render-target");if(!r)return;let n=t.querySelector("iframe"),a=n?.contentWindow;if(a)return e(a)}}function v(e){let t=e.data;if(!t)return;if("string"==typeof t)try{t=JSON.parse(t)}catch{return}if("object"!=typeof t&&void 0!=t||"render"!==t.type||"string"!=typeof t.identity)return;let r=t.identity;if("string"!=typeof t.body)return;let n=t.body,o=y(document,r);if(!o||e.origin!==o.getAttribute("data-host"))return;let i=null!=t.payload?t.payload:void 0,l=o.querySelector("iframe"),s=l?.contentWindow;switch(n){case"hello":{let e=c.get(o)||{untimed:!0};if(e.hello=Date.now(),!s)return;g(s,{type:"render:cmd",body:{cmd:"ack",ack:!0}}),g(s,{type:"render:cmd",body:{cmd:"branding",branding:!1}});break}case"error":f(o,i?.error);break;case"error:fatal":f(o,i?.error),o.classList.add("is-render-failed-fatal");break;case"error:invalid":f(o,i?.error),o.classList.add("is-render-failed-invalid");break;case"loading":o.classList.remove(...d),o.classList.add("is-render-loading");break;case"loaded":o.classList.remove(...d),o.classList.add("is-render-loaded");break;case"ready":(0,a.I)(o),o.classList.remove(...d),o.classList.add("is-render-ready"),i&&"number"==typeof i.height&&(o.style.height=`${i.height}px`,""!==location.hash&&window.dispatchEvent(new HashChangeEvent("hashchange")));break;case"resize":i&&"number"==typeof i.height&&(o.style.height=`${i.height}px`);break;case"code_rendering_service:container:get_size":g(s,{type:"render:cmd",body:{cmd:"code_rendering_service:container:size","code_rendering_service:container:size":{width:o?.getBoundingClientRect().width}}});break;case"code_rendering_service:markdown:get_data":if(!s)return;!function(){let e;let t=l?.getAttribute("data-content")??"";try{e=JSON.parse(t)?.data}catch{e=null}if(!e)return;let r={type:"render:cmd",body:{cmd:"code_rendering_service:data:ready","code_rendering_service:data:ready":{data:e,width:o?.getBoundingClientRect().width}}};g(s,r)}()}}(0,o.N7)(".js-render-target",function(e){let t=e;t.classList.remove(...s),t.style.height="auto",!c.get(e)?.load&&(u(e),c.get(e)||(c.set(e,{load:Date.now(),hello:null,helloTimer:window.setTimeout(p,1e4,e,!0),loadTimer:window.setTimeout(p,45e3,e)}),e.classList.add("is-render-automatic","is-render-requested")))}),window.addEventListener("message",v);let w=b(e=>g(e,{type:"render:cmd",body:{cmd:"code_rendering_service:behaviour:expand_all"}})),L=b(e=>g(e,{type:"render:cmd",body:{cmd:"code_rendering_service:behaviour:collapse_all"}}))},76237:(e,t,r)=>{r.d(t,{Fg:()=>f,I3:()=>i,h5:()=>s,on:()=>c,yn:()=>u});var n=r(4412),a=r(67404);function o(){(0,a.d8)("preferred_color_mode",i())}function i(){return l("dark")?"dark":l("light")?"light":void 0}function l(e){return window.matchMedia&&window.matchMedia(`(prefers-color-scheme: ${e})`).matches}function d(){let e=document.querySelector("html[data-color-mode]");if(e)return e.getAttribute("data-color-mode")}function s(e){let t=document.querySelector("html[data-color-mode]");t&&t.setAttribute("data-color-mode",e)}function c(e,t){let r=document.querySelector("html[data-color-mode]");r&&r.setAttribute(`data-${t}-theme`,e)}function u(e){let t=document.querySelector("html[data-color-mode]");if(t)return t.getAttribute(`data-${e}-theme`)}function f(e="light"){let t=d();return("auto"===t?i():t)??e}(async()=>{if(await n.x,o(),window.matchMedia){let e=window.matchMedia("(prefers-color-scheme: dark)");e?.addEventListener?e.addEventListener("change",o):e.addListener(o)}})()},67404:(e,t,r)=>{function n(e){return a(e)[0]}function a(e){let t=[];for(let r of o()){let[n,a]=r.trim().split("=");e===n&&void 0!==a&&t.push({key:n,value:a})}return t}function o(){try{return document.cookie.split(";")}catch{return[]}}function i(e,t,r=null,n=!1,a="lax"){let o=document.domain;if(null==o)throw Error("Unable to get document domain");o.endsWith(".github.com")&&(o="github.com");let i="https:"===location.protocol?"; secure":"",l=r?`; expires=${r}`:"";!1===n&&(o=`.${o}`);try{document.cookie=`${e}=${t}; path=/; domain=${o}${l}${i}; samesite=${a}`}catch{}}function l(e,t=!1){let r=document.domain;if(null==r)throw Error("Unable to get document domain");r.endsWith(".github.com")&&(r="github.com");let n=new Date().getTime(),a=new Date(n-1).toUTCString(),o="https:"===location.protocol?"; secure":"",i=`; expires=${a}`;!1===t&&(r=`.${r}`);try{document.cookie=`${e}=''; path=/; domain=${r}${i}${o}`}catch{}}r.d(t,{$1:()=>a,d8:()=>i,ej:()=>n,kT:()=>l})},9302:(e,t,r)=>{let n,a;r.d(t,{lO:()=>h,qA:()=>f,y0:()=>l});let o=[],i=0;function l(){return n}function d(){try{return Math.min(Math.max(0,history.length)||0,9007199254740991)}catch(e){return 0}}function s(){return d()-1+i}function c(e){n=e;let t=location.href;o[s()]={url:t,state:n},o.length=d(),window.dispatchEvent(new CustomEvent("statechange",{bubbles:!1,cancelable:!1}))}function u(){return new Date().getTime()}function f(e,t,r){i=0;let n={_id:u(),...e};history.pushState(n,t,r),c(n)}function h(e,t,r){let n={...l(),...e};history.replaceState(n,t,r),c(n)}n=function(){let e={_id:new Date().getTime(),...history.state};return c(e),e}(),window.addEventListener("popstate",function(e){let t=e.state;if(!t||!t._id&&!t.turbo?.restorationIdentifier)return;let r=t.turbo?.restorationIdentifier,n=o[s()-1]?.state?.turbo?.restorationIdentifier;n===r?i--:i++,c(t)},!0),window.addEventListener("turbo:visit",e=>{e instanceof CustomEvent&&(a=e.detail.action)}),window.addEventListener("turbo:load",()=>{"restore"!==a&&(i=0,h(history.state,"",""))}),window.addEventListener("hashchange",function(){if(d()>o.length){let e={_id:u()};history.replaceState(e,"",location.href),c(e)}},!0),window.addEventListener("pageshow",()=>{o=[],i=0})},22971:(e,t,r)=>{r.d(t,{H5:()=>u,Of:()=>m,x0:()=>h,z8:()=>f});var n=r(64611),a=r(36071),o=r(5875),i=r(67525),l=r(96776),d=r(9302);let s=new WeakMap,c={};function u(){for(let e of Object.keys(c))delete c[e];let e=history.state||{};e.staleRecords=c,(0,d.lO)(e,"",location.href),window.location.reload()}function f(){if(Object.keys(c).length>0){let e=history.state||{};e.staleRecords=c,(0,d.lO)(e,"",location.href)}}async function h(e){if(s.get(e))return;let t=e.hasAttribute("data-retain-focus"),r=e.getAttribute("data-url");if(!r)throw Error("could not get url");let a=new AbortController;s.set(e,a);try{let o=await fetch(r,{signal:a.signal,headers:{Accept:"text/html","X-Requested-With":"XMLHttpRequest"}});if(!o.ok)return;let i=await o.text();if((0,n.M)(e,t)){console.warn("Failed to update content with interactions",e);return}return c[r]=i,p(e,i,t)}catch{}finally{s.delete(e)}}async function m(e,t,r=!1){let n=s.get(e);n?.abort();let a=e.closest(".js-updatable-content[data-url], .js-updatable-content [data-url]");return!r&&a&&a===e&&(c[a.getAttribute("data-url")||""]=t),p(e,t)}function p(e,t,r=!1){return(0,l._8)(document,()=>{let n=(0,i.r)(document,t.trim()),a=r&&e.ownerDocument&&e===e.ownerDocument.activeElement?n.querySelector("*"):null,l=Array.from(e.querySelectorAll("details[open][id]")).map(e=>e.id);for(let t of("DETAILS"===e.tagName&&e.id&&e.hasAttribute("open")&&l.push(e.id),e.querySelectorAll(".js-updatable-content-preserve-scroll-position"))){let e=t.getAttribute("data-updatable-content-scroll-position-id")||"";g.set(e,t.scrollTop)}for(let e of l){let t=n.querySelector(`#${e}`);t&&t.setAttribute("open","")}(0,o.nn)(e,n),a instanceof HTMLElement&&a.focus()})}let g=new Map;(0,a.N7)(".js-updatable-content-preserve-scroll-position",{constructor:HTMLElement,add(e){let t=e.getAttribute("data-updatable-content-scroll-position-id");if(!t)return;let r=g.get(t);null!=r&&(e.scrollTop=r)}})},75198:(e,t,r)=>{r.r(t),r.d(t,{softNavigate:()=>o});var n=r(67852),a=r(68202);function o(e,t){(0,a.LD)("turbo"),(0,n.visit)(e,{...t})}}}]);
//# sourceMappingURL=app_assets_modules_github_behaviors_ajax-error_ts-app_assets_modules_github_behaviors_include-f12a82-34d1e42f4092.js.map