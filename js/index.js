ready(() => {
  $('.owl-carousel').owlCarousel({
    lazyLoad: true,
    loop: true,
    margin: 10,
    dots: false,
    autoplay: true,
    autoplayTimeout: 6000,
    autoplayHoverPause: true,
    nav: true,
    navText : ['<i class="fa fa-angle-left" aria-hidden="true"></i>','<i class="fa fa-angle-right" aria-hidden="true"></i>'],
    margin:10,
    center: true,
  });
});
