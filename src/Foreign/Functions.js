exports.toggleOverflowClass = function (htmlElement) {
  return function () {
    // htmlElement.classList.toggle(
    //   "overflowing",
    //   this.scrollWidth > this.clientWidth
    // );
    htmlElement.classList.toggle(
      "overflowing",
      htmlElement.scrollWidth > htmlElement.clientWidth
    );
  };
};

exports.attachReelObserver = function (htmlElement) {
  return function () {
    new MutationObserver((entries) => {
      thisEl = entries[0].target;
      thisEl.classList.toggle(
        "overflowing",
        thisEl.scrollWidth > thisEl.clientWidth
      );
    }).observe(htmlElement, {
      childList: true,
    });

    new ResizeObserver((entries) => {
      thisEl = entries[0].target;
      thisEl.classList.toggle(
        "overflowing",
        thisEl.scrollWidth > thisEl.clientWidth
      );
    }).observe(htmlElement);
  };
};
